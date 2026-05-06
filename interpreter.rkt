#lang racket

(require "simpleParser.rkt")

(provide interpret
         interpret-tree
         empty-state)

;; Java-lite interpreter, Stage 1.
;; The interpreter is split into state, value, boolean, and statement helpers so
;; future stages can change the state representation without rewriting the
;; evaluator.

(define return-name 'return)
(define unassigned-value 'unassigned)

;; ---------------------------------------------------------------------------
;; State operations

(define empty-state '())

(define state-declared?
  (lambda (name state)
    (cond
      [(null? state) #f]
      [(eq? name (caar state)) #t]
      [else (state-declared? name (cdr state))])))

(define state-add
  (lambda (name value state)
    (if (state-declared? name state)
        (error 'M_state "variable already declared: ~a" name)
        (cons (list name value) state))))

(define state-lookup-binding
  (lambda (name state)
    (cond
      [(null? state) (error 'M_state "variable used before declaration: ~a" name)]
      [(eq? name (caar state)) (car state)]
      [else (state-lookup-binding name (cdr state))])))

(define state-lookup
  (lambda (name state)
    (let ([value (cadr (state-lookup-binding name state))])
      (if (eq? value unassigned-value)
          (error 'M_state "variable used before assignment: ~a" name)
          value))))

(define state-update
  (lambda (name value state)
    (cond
      [(null? state) (error 'M_state "variable assigned before declaration: ~a" name)]
      [(eq? name (caar state)) (cons (list name value) (cdr state))]
      [else (cons (car state) (state-update name value (cdr state)))])))

(define state-returned?
  (lambda (state)
    (state-declared? return-name state)))

(define state-set-return
  (lambda (value state)
    (if (state-declared? return-name state)
        (state-update return-name value state)
        (state-add return-name value state))))

;; ---------------------------------------------------------------------------
;; Result helpers for expression evaluation.

(define make-result
  (lambda (value state)
    (list value state)))

(define result-value car)
(define result-state cadr)

;; ---------------------------------------------------------------------------
;; Boolean operations

(define M_boolean-from-racket
  (lambda (value)
    (if value 'true 'false)))

(define M_boolean-true?
  (lambda (value)
    (eq? value 'true)))

(define M_boolean-not
  (lambda (value)
    (M_boolean-from-racket (not (M_boolean-true? value)))))

(define M_boolean-and
  (lambda (left right)
    (M_boolean-from-racket (and (M_boolean-true? left)
                                (M_boolean-true? right)))))

(define M_boolean-or
  (lambda (left right)
    (M_boolean-from-racket (or (M_boolean-true? left)
                               (M_boolean-true? right)))))

;; ---------------------------------------------------------------------------
;; Value operations

(define M_value-binary
  (lambda (operator left right)
    (case operator
      [(+) (+ left right)]
      [(-) (- left right)]
      [(*) (* left right)]
      [(/) (quotient left right)]
      [(%) (remainder left right)]
      [(==) (M_boolean-from-racket (equal? left right))]
      [(!=) (M_boolean-from-racket (not (equal? left right)))]
      [(<) (M_boolean-from-racket (< left right))]
      [(>) (M_boolean-from-racket (> left right))]
      [(<=) (M_boolean-from-racket (<= left right))]
      [(>=) (M_boolean-from-racket (>= left right))]
      [(&&) (M_boolean-and left right)]
      [(||) (M_boolean-or left right)]
      [else (error 'M_value "unknown binary operator: ~a" operator)])))

(define M_value
  (lambda (expression state)
    (cond
      [(number? expression) (make-result expression state)]
      [(eq? expression 'true) (make-result 'true state)]
      [(eq? expression 'false) (make-result 'false state)]
      [(symbol? expression) (make-result (state-lookup expression state) state)]
      [(and (pair? expression) (eq? '= (car expression)))
       (let* ([value-result (M_value (caddr expression) state)]
              [new-state (state-update (cadr expression)
                                       (result-value value-result)
                                       (result-state value-result))])
         (make-result (result-value value-result) new-state))]
      [(and (pair? expression) (eq? '- (car expression)) (null? (cddr expression)))
       (let ([value-result (M_value (cadr expression) state)])
         (make-result (- (result-value value-result)) (result-state value-result)))]
      [(and (pair? expression) (eq? '! (car expression)))
       (let ([value-result (M_value (cadr expression) state)])
         (make-result (M_boolean-not (result-value value-result))
                      (result-state value-result)))]
      [(pair? expression)
       (let* ([left-result (M_value (cadr expression) state)]
              [right-result (M_value (caddr expression) (result-state left-result))])
         (make-result (M_value-binary (car expression)
                                      (result-value left-result)
                                      (result-value right-result))
                      (result-state right-result)))]
      [else (error 'M_value "unknown expression: ~a" expression)])))

;; ---------------------------------------------------------------------------
;; Statement operations

(define M_state-declare
  (lambda (statement state)
    (if (null? (cddr statement))
        (state-add (cadr statement) unassigned-value state)
        (let ([value-result (M_value (caddr statement) state)])
          (state-add (cadr statement)
                     (result-value value-result)
                     (result-state value-result))))))

(define M_state-assign
  (lambda (statement state)
    (result-state (M_value statement state))))

(define M_state-return
  (lambda (statement state)
    (let ([value-result (M_value (cadr statement) state)])
      (state-set-return (result-value value-result)
                        (result-state value-result)))))

(define M_state-if
  (lambda (statement state)
    (let ([condition-result (M_value (cadr statement) state)])
      (cond
        [(M_boolean-true? (result-value condition-result))
         (M_state (caddr statement) (result-state condition-result))]
        [(null? (cdddr statement)) (result-state condition-result)]
        [else (M_state (cadddr statement) (result-state condition-result))]))))

(define M_state-while
  (lambda (statement state)
    (let ([condition-result (M_value (cadr statement) state)])
      (cond
        [(not (M_boolean-true? (result-value condition-result)))
         (result-state condition-result)]
        [else
         (let ([body-state (M_state (caddr statement)
                                    (result-state condition-result))])
           (if (state-returned? body-state)
               body-state
               (M_state-while statement body-state)))]))))

(define M_state-block
  (lambda (statements state)
    (M_state-list statements state)))

(define M_state
  (lambda (statement state)
    (case (car statement)
      [(var) (M_state-declare statement state)]
      [(=) (M_state-assign statement state)]
      [(return) (M_state-return statement state)]
      [(if) (M_state-if statement state)]
      [(while) (M_state-while statement state)]
      [(begin) (M_state-block (cdr statement) state)]
      [else (error 'M_state "unknown statement: ~a" statement)])))

(define M_state-list
  (lambda (statements state)
    (cond
      [(null? statements) state]
      [(state-returned? state) state]
      [else (M_state-list (cdr statements)
                          (M_state (car statements) state))])))

;; ---------------------------------------------------------------------------
;; Public entry points

(define interpret-tree
  (lambda (syntax-tree)
    (state-lookup return-name
                  (M_state-list syntax-tree empty-state))))

(define interpret
  (lambda (filename)
    (interpret-tree (parser filename))))

#lang racket

(require "simpleParser.rkt")

(provide interpret
         interpret-tree
         empty-state)

;; Java-lite interpreter, Stage 2.
;; Statement evaluation uses continuation-passing style so return, break,
;; continue, and throw can leave nested blocks while preserving scope cleanup.

(define unassigned-value 'unassigned)

;; ---------------------------------------------------------------------------
;; State operations

(define empty-state (list '()))

(define state-current-layer car)
(define state-enclosing-layers cdr)

(define state-enter-scope
  (lambda (state)
    (cons '() state)))

(define state-exit-scope
  (lambda (state)
    (if (null? (state-enclosing-layers state))
        (error 'M_state "cannot exit the global scope")
        (state-enclosing-layers state))))

(define state-declared-in-layer?
  (lambda (name layer)
    (cond
      [(null? layer) #f]
      [(eq? name (caar layer)) #t]
      [else (state-declared-in-layer? name (cdr layer))])))

(define state-add
  (lambda (name value state)
    (if (state-declared-in-layer? name (state-current-layer state))
        (error 'M_state "variable already declared: ~a" name)
        (cons (cons (list name value) (state-current-layer state))
              (state-enclosing-layers state)))))

(define state-lookup-in-layer
  (lambda (name layer)
    (cond
      [(null? layer) #f]
      [(eq? name (caar layer)) (car layer)]
      [else (state-lookup-in-layer name (cdr layer))])))

(define state-lookup-binding
  (lambda (name state)
    (cond
      [(null? state) (error 'M_state "variable used before declaration: ~a" name)]
      [(state-lookup-in-layer name (state-current-layer state))
       => (lambda (binding) binding)]
      [else (state-lookup-binding name (state-enclosing-layers state))])))

(define state-lookup
  (lambda (name state)
    (let ([value (cadr (state-lookup-binding name state))])
      (if (eq? value unassigned-value)
          (error 'M_state "variable used before assignment: ~a" name)
          value))))

(define state-update-layer
  (lambda (name value layer)
    (cond
      [(null? layer) #f]
      [(eq? name (caar layer)) (cons (list name value) (cdr layer))]
      [else
       (let ([updated-rest (state-update-layer name value (cdr layer))])
         (if updated-rest
             (cons (car layer) updated-rest)
             #f))])))

(define state-update
  (lambda (name value state)
    (cond
      [(null? state) (error 'M_state "variable assigned before declaration: ~a" name)]
      [(state-update-layer name value (state-current-layer state))
       => (lambda (updated-layer)
            (cons updated-layer (state-enclosing-layers state)))]
      [else
       (cons (state-current-layer state)
             (state-update name value (state-enclosing-layers state)))])))

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
  (lambda (statement state next-k)
    (if (null? (cddr statement))
        (next-k (state-add (cadr statement) unassigned-value state))
        (let ([value-result (M_value (caddr statement) state)])
          (next-k (state-add (cadr statement)
                             (result-value value-result)
                             (result-state value-result)))))))

(define M_state-assign
  (lambda (statement state next-k)
    (next-k (result-state (M_value statement state)))))

(define M_state-return
  (lambda (statement state return-k)
    (let ([value-result (M_value (cadr statement) state)])
      (return-k (result-value value-result)
                (result-state value-result)))))

(define M_state-throw
  (lambda (statement state throw-k)
    (let ([value-result (M_value (cadr statement) state)])
      (throw-k (result-value value-result)
               (result-state value-result)))))

(define M_state-if
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (let ([condition-result (M_value (cadr statement) state)])
      (cond
        [(M_boolean-true? (result-value condition-result))
         (M_state (caddr statement)
                  (result-state condition-result)
                  return-k break-k continue-k throw-k next-k)]
        [(null? (cdddr statement)) (next-k (result-state condition-result))]
        [else
         (M_state (cadddr statement)
                  (result-state condition-result)
                  return-k break-k continue-k throw-k next-k)]))))

(define M_state-while
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (let ([condition-result (M_value (cadr statement) state)])
      (if (M_boolean-true? (result-value condition-result))
          (M_state (caddr statement)
                   (result-state condition-result)
                   return-k
                   (lambda (break-state) (next-k break-state))
                   (lambda (continue-state)
                     (M_state-while statement continue-state
                                    return-k break-k continue-k throw-k next-k))
                   throw-k
                   (lambda (body-state)
                     (M_state-while statement body-state
                                    return-k break-k continue-k throw-k next-k)))
          (next-k (result-state condition-result))))))

(define M_state-block
  (lambda (statements state return-k break-k continue-k throw-k next-k)
    (let ([block-state (state-enter-scope state)])
      (M_state-list statements
                    block-state
                    (lambda (value return-state)
                      (return-k value (state-exit-scope return-state)))
                    (lambda (break-state)
                      (break-k (state-exit-scope break-state)))
                    (lambda (continue-state)
                      (continue-k (state-exit-scope continue-state)))
                    (lambda (value throw-state)
                      (throw-k value (state-exit-scope throw-state)))
                    (lambda (normal-state)
                      (next-k (state-exit-scope normal-state)))))))

(define catch-clause?
  (lambda (catch-clause)
    (and (pair? catch-clause) (eq? 'catch (car catch-clause)))))

(define finally-clause?
  (lambda (finally-clause)
    (and (pair? finally-clause) (eq? 'finally (car finally-clause)))))

(define M_state-finally
  (lambda (finally-clause state return-k break-k continue-k throw-k resume-k)
    (if (finally-clause? finally-clause)
        (M_state-block (cadr finally-clause)
                       state
                       return-k break-k continue-k throw-k resume-k)
        (resume-k state))))

(define M_state-catch
  (lambda (catch-clause thrown-value state return-k break-k continue-k throw-k next-k)
    (if (catch-clause? catch-clause)
        (let* ([catch-name (caadr catch-clause)]
               [catch-state (state-add catch-name
                                       thrown-value
                                       (state-enter-scope state))])
          (M_state-list (caddr catch-clause)
                        catch-state
                        (lambda (value return-state)
                          (return-k value (state-exit-scope return-state)))
                        (lambda (break-state)
                          (break-k (state-exit-scope break-state)))
                        (lambda (continue-state)
                          (continue-k (state-exit-scope continue-state)))
                        (lambda (value throw-state)
                          (throw-k value (state-exit-scope throw-state)))
                        (lambda (normal-state)
                          (next-k (state-exit-scope normal-state)))))
        (throw-k thrown-value state))))

(define M_state-try
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (let ([try-body (cadr statement)]
          [catch-clause (caddr statement)]
          [finally-clause (cadddr statement)])
      (M_state-block try-body
                     state
                     (lambda (value return-state)
                       (M_state-finally finally-clause
                                        return-state
                                        return-k break-k continue-k throw-k
                                        (lambda (finally-state)
                                          (return-k value finally-state))))
                     (lambda (break-state)
                       (M_state-finally finally-clause
                                        break-state
                                        return-k break-k continue-k throw-k
                                        break-k))
                     (lambda (continue-state)
                       (M_state-finally finally-clause
                                        continue-state
                                        return-k break-k continue-k throw-k
                                        continue-k))
                     (lambda (value throw-state)
                       (if (catch-clause? catch-clause)
                           (M_state-catch catch-clause
                                          value
                                          throw-state
                                          (lambda (catch-value catch-return-state)
                                            (M_state-finally finally-clause
                                                             catch-return-state
                                                             return-k break-k continue-k throw-k
                                                             (lambda (finally-state)
                                                               (return-k catch-value finally-state))))
                                          (lambda (catch-break-state)
                                            (M_state-finally finally-clause
                                                             catch-break-state
                                                             return-k break-k continue-k throw-k
                                                             break-k))
                                          (lambda (catch-continue-state)
                                            (M_state-finally finally-clause
                                                             catch-continue-state
                                                             return-k break-k continue-k throw-k
                                                             continue-k))
                                          (lambda (catch-value catch-throw-state)
                                            (M_state-finally finally-clause
                                                             catch-throw-state
                                                             return-k break-k continue-k throw-k
                                                             (lambda (finally-state)
                                                               (throw-k catch-value finally-state))))
                                          (lambda (catch-normal-state)
                                            (M_state-finally finally-clause
                                                             catch-normal-state
                                                             return-k break-k continue-k throw-k
                                                             next-k)))
                           (M_state-finally finally-clause
                                            throw-state
                                            return-k break-k continue-k throw-k
                                            (lambda (finally-state)
                                              (throw-k value finally-state)))))
                     (lambda (normal-state)
                       (M_state-finally finally-clause
                                        normal-state
                                        return-k break-k continue-k throw-k
                                        next-k))))))

(define M_state
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (case (car statement)
      [(var) (M_state-declare statement state next-k)]
      [(=) (M_state-assign statement state next-k)]
      [(return) (M_state-return statement state return-k)]
      [(break) (break-k state)]
      [(continue) (continue-k state)]
      [(throw) (M_state-throw statement state throw-k)]
      [(if) (M_state-if statement state return-k break-k continue-k throw-k next-k)]
      [(while) (M_state-while statement state return-k break-k continue-k throw-k next-k)]
      [(begin) (M_state-block (cdr statement) state return-k break-k continue-k throw-k next-k)]
      [(try) (M_state-try statement state return-k break-k continue-k throw-k next-k)]
      [else (error 'M_state "unknown statement: ~a" statement)])))

(define M_state-list
  (lambda (statements state return-k break-k continue-k throw-k next-k)
    (if (null? statements)
        (next-k state)
        (M_state (car statements)
                 state
                 return-k break-k continue-k throw-k
                 (lambda (next-state)
                   (M_state-list (cdr statements)
                                 next-state
                                 return-k break-k continue-k throw-k next-k))))))

;; ---------------------------------------------------------------------------
;; Public entry points

(define interpret-tree
  (lambda (syntax-tree)
    (M_state-list syntax-tree
                  empty-state
                  (lambda (value state) value)
                  (lambda (state) (error 'M_state "break used outside of a loop"))
                  (lambda (state) (error 'M_state "continue used outside of a loop"))
                  (lambda (value state) (error 'M_state "uncaught exception: ~a" value))
                  (lambda (state) (error 'M_state "program ended without return")))))

(define interpret
  (lambda (filename)
    (interpret-tree (parser filename))))

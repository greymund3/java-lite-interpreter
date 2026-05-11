#lang racket

(require "functionParser.rkt")

(provide interpret
         interpret-tree
         empty-state)

;; Java-lite interpreter, Stage 3.
;; Variables and functions live in layered environments. Variable bindings use
;; boxes so global and closure side effects work naturally across function calls.

(struct closure (params body env) #:transparent)

(define unassigned-value 'unassigned)

;; ---------------------------------------------------------------------------
;; Environment operations

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

(define state-add-cell
  (lambda (name cell state)
    (if (state-declared-in-layer? name (state-current-layer state))
        (error 'M_state "name already declared: ~a" name)
        (cons (cons (list name cell) (state-current-layer state))
              (state-enclosing-layers state)))))

(define state-add
  (lambda (name value state)
    (state-add-cell name (box value) state)))

(define state-lookup-in-layer
  (lambda (name layer)
    (cond
      [(null? layer) #f]
      [(eq? name (caar layer)) (car layer)]
      [else (state-lookup-in-layer name (cdr layer))])))

(define state-lookup-binding
  (lambda (name state)
    (cond
      [(null? state) (error 'M_state "name used before declaration: ~a" name)]
      [(state-lookup-in-layer name (state-current-layer state))
       => (lambda (binding) binding)]
      [else (state-lookup-binding name (state-enclosing-layers state))])))

(define state-lookup-cell
  (lambda (name state)
    (cadr (state-lookup-binding name state))))

(define state-lookup
  (lambda (name state)
    (let ([value (unbox (state-lookup-cell name state))])
      (if (eq? value unassigned-value)
          (error 'M_state "name used before initialization: ~a" name)
          value))))

(define state-update
  (lambda (name value state)
    (begin
      (set-box! (state-lookup-cell name state) value)
      state)))

(define function-definition?
  (lambda (statement)
    (and (pair? statement) (eq? 'function (car statement)))))

(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)

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
;; Function parameter helpers

(define make-param
  (lambda (mode name)
    (list mode name)))

(define param-mode car)
(define param-name cadr)

(define parse-params
  (lambda (params)
    (cond
      [(null? params) '()]
      [(eq? (car params) '&)
       (if (or (null? (cdr params)) (eq? (cadr params) '&))
           (error 'M_value "missing reference parameter name")
           (cons (make-param 'reference (cadr params))
                 (parse-params (cddr params))))]
      [else
       (cons (make-param 'value (car params))
             (parse-params (cdr params)))])))

(define bind-arguments
  (lambda (params args caller-state function-state return-k throw-k)
    (cond
      [(and (null? params) (null? args)) (return-k function-state)]
      [(or (null? params) (null? args))
       (error 'M_value "wrong number of arguments")]
      [(eq? 'reference (param-mode (car params)))
       (let ([arg (car args)])
         (if (symbol? arg)
             (bind-arguments (cdr params)
                             (cdr args)
                             caller-state
                             (state-add-cell (param-name (car params))
                                             (state-lookup-cell arg caller-state)
                                             function-state)
                             return-k
                             throw-k)
             (error 'M_value "reference argument must be a variable: ~a" arg)))]
      [else
       (M_value (car args)
                caller-state
                (lambda (value next-caller-state)
                  (bind-arguments (cdr params)
                                  (cdr args)
                                  next-caller-state
                                  (state-add (param-name (car params))
                                             value
                                             function-state)
                                  return-k
                                  throw-k))
                throw-k)])))

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

(define M_value-funcall
  (lambda (expression state return-k throw-k)
    (let ([function-value (state-lookup (cadr expression) state)])
      (if (not (closure? function-value))
          (error 'M_value "attempted to call a non-function: ~a" (cadr expression))
          (let ([function-state (state-enter-scope (closure-env function-value))])
            (bind-arguments (parse-params (closure-params function-value))
                            (cddr expression)
                            state
                            function-state
                            (lambda (call-state)
                              (M_state-list (closure-body function-value)
                                            call-state
                                            (lambda (value return-state)
                                              (return-k value state))
                                            (lambda (break-state)
                                              (error 'M_state "break used outside of a loop"))
                                            (lambda (continue-state)
                                              (error 'M_state "continue used outside of a loop"))
                                            (lambda (value throw-state)
                                              (throw-k value state))
                                            (lambda (normal-state)
                                              (return-k unassigned-value state))))
                            throw-k))))))

(define M_value
  (lambda (expression state return-k throw-k)
    (cond
      [(number? expression) (return-k expression state)]
      [(eq? expression 'true) (return-k 'true state)]
      [(eq? expression 'false) (return-k 'false state)]
      [(symbol? expression) (return-k (state-lookup expression state) state)]
      [(and (pair? expression) (eq? '= (car expression)))
       (M_value (caddr expression)
                state
                (lambda (value value-state)
                  (return-k value (state-update (cadr expression) value value-state)))
                throw-k)]
      [(and (pair? expression) (eq? 'funcall (car expression)))
       (M_value-funcall expression state return-k throw-k)]
      [(and (pair? expression) (eq? '- (car expression)) (null? (cddr expression)))
       (M_value (cadr expression)
                state
                (lambda (value value-state)
                  (return-k (- value) value-state))
                throw-k)]
      [(and (pair? expression) (eq? '! (car expression)))
       (M_value (cadr expression)
                state
                (lambda (value value-state)
                  (return-k (M_boolean-not value) value-state))
                throw-k)]
      [(pair? expression)
       (M_value (cadr expression)
                state
                (lambda (left left-state)
                  (M_value (caddr expression)
                           left-state
                           (lambda (right right-state)
                             (return-k (M_value-binary (car expression) left right)
                                       right-state))
                           throw-k))
                throw-k)]
      [else (error 'M_value "unknown expression: ~a" expression)])))

;; ---------------------------------------------------------------------------
;; Statement operations

(define M_state-declare
  (lambda (statement state return-k throw-k next-k)
    (if (null? (cddr statement))
        (next-k (state-add (cadr statement) unassigned-value state))
        (M_value (caddr statement)
                 state
                 (lambda (value value-state)
                   (next-k (state-add (cadr statement) value value-state)))
                 throw-k))))

(define M_state-assign
  (lambda (statement state throw-k next-k)
    (M_value statement state
             (lambda (value value-state) (next-k value-state))
             throw-k)))

(define M_state-return
  (lambda (statement state return-k throw-k)
    (M_value (cadr statement)
             state
             (lambda (value value-state)
               (return-k value value-state))
             throw-k)))

(define M_state-throw
  (lambda (statement state throw-k)
    (M_value (cadr statement)
             state
             (lambda (value value-state)
               (throw-k value value-state))
             throw-k)))

(define M_state-function
  (lambda (statement state next-k)
    (next-k (state-update (function-name statement)
                          (closure (function-params statement)
                                   (function-body statement)
                                   state)
                          state))))

(define M_state-if
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (M_value (cadr statement)
             state
             (lambda (condition condition-state)
               (cond
                 [(M_boolean-true? condition)
                  (M_state (caddr statement)
                           condition-state
                           return-k break-k continue-k throw-k next-k)]
                 [(null? (cdddr statement)) (next-k condition-state)]
                 [else
                  (M_state (cadddr statement)
                           condition-state
                           return-k break-k continue-k throw-k next-k)]))
             throw-k)))

(define M_state-while
  (lambda (statement state return-k break-k continue-k throw-k next-k)
    (M_value (cadr statement)
             state
             (lambda (condition condition-state)
               (if (M_boolean-true? condition)
                   (M_state (caddr statement)
                            condition-state
                            return-k
                            (lambda (break-state) (next-k break-state))
                            (lambda (continue-state)
                              (M_state-while statement continue-state
                                             return-k break-k continue-k throw-k next-k))
                            throw-k
                            (lambda (body-state)
                              (M_state-while statement body-state
                                             return-k break-k continue-k throw-k next-k)))
                   (next-k condition-state)))
             throw-k)))

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
      [(var) (M_state-declare statement state return-k throw-k next-k)]
      [(=) (M_state-assign statement state throw-k next-k)]
      [(funcall) (M_value statement state
                          (lambda (value value-state) (next-k value-state))
                          throw-k)]
      [(function) (M_state-function statement state next-k)]
      [(return) (M_state-return statement state return-k throw-k)]
      [(break) (break-k state)]
      [(continue) (continue-k state)]
      [(throw) (M_state-throw statement state throw-k)]
      [(if) (M_state-if statement state return-k break-k continue-k throw-k next-k)]
      [(while) (M_state-while statement state return-k break-k continue-k throw-k next-k)]
      [(begin) (M_state-block (cdr statement) state return-k break-k continue-k throw-k next-k)]
      [(try) (M_state-try statement state return-k break-k continue-k throw-k next-k)]
      [else (error 'M_state "unknown statement: ~a" statement)])))

(define M_state-add-function-names
  (lambda (statements state)
    (cond
      [(null? statements) state]
      [(function-definition? (car statements))
       (M_state-add-function-names
        (cdr statements)
        (state-add (function-name (car statements)) unassigned-value state))]
      [else (M_state-add-function-names (cdr statements) state)])))

(define M_state-install-function-closures
  (lambda (statements state)
    (cond
      [(null? statements) state]
      [(function-definition? (car statements))
       (M_state-install-function-closures
        (cdr statements)
        (state-update (function-name (car statements))
                      (closure (function-params (car statements))
                               (function-body (car statements))
                               state)
                      state))]
      [else (M_state-install-function-closures (cdr statements) state)])))

(define M_state-hoist-functions
  (lambda (statements state)
    (M_state-install-function-closures
     statements
     (M_state-add-function-names statements state))))

(define M_state-list
  (lambda (statements state return-k break-k continue-k throw-k next-k)
    (let ([hoisted-state (M_state-hoist-functions statements state)])
      (letrec ([execute
                (lambda (remaining current-state)
                  (cond
                    [(null? remaining) (next-k current-state)]
                    [else
                     (M_state (car remaining)
                              current-state
                              return-k break-k continue-k throw-k
                              (lambda (next-state)
                                (execute (cdr remaining) next-state)))]))])
        (execute statements hoisted-state)))))

;; ---------------------------------------------------------------------------
;; Top-level program operations

(define M_state-top-statements
  (lambda (statements state next-k throw-k)
    (cond
      [(null? statements) (next-k state)]
      [(function-definition? (car statements))
       (M_state-top-statements
        (cdr statements)
        (state-update (function-name (car statements))
                      (closure (function-params (car statements))
                               (function-body (car statements))
                               state)
                      state)
        next-k
        throw-k)]
      [(eq? 'var (caar statements))
       (M_state-declare (car statements)
                        state
                        (lambda (value state)
                          (error 'M_state "return used outside of a function"))
                        throw-k
                        (lambda (next-state)
                          (M_state-top-statements (cdr statements) next-state next-k throw-k)))]
      [else (error 'M_state "illegal top-level statement: ~a" (car statements))])))

(define M_state-top
  (lambda (statements state next-k throw-k)
    (M_state-top-statements
     statements
     (M_state-add-function-names statements state)
     next-k
     throw-k)))

(define call-main
  (lambda (state)
    (M_value '(funcall main)
             state
             (lambda (value value-state) value)
             (lambda (value throw-state)
               (error 'M_state "uncaught exception: ~a" value)))))

;; ---------------------------------------------------------------------------
;; Public entry points

(define interpret-tree
  (lambda (syntax-tree)
    (M_state-top syntax-tree
                 empty-state
                 call-main
                 (lambda (value throw-state)
                   (error 'M_state "uncaught exception: ~a" value)))))

(define interpret
  (lambda (filename)
    (interpret-tree (parser filename))))

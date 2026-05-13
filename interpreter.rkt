#lang racket

(require "classParser.rkt")

(provide interpret
         interpret-tree
         empty-state)

;; Java-lite interpreter, Stage 4.
;; Adds class closures, object instances, fields, methods, this/super, and
;; dynamic dispatch while preserving the continuation-based control flow from
;; earlier stages.

(struct closure (params body env owner static? captured-this) #:transparent)
(struct class-closure (name parent fields methods static-methods static-fields) #:transparent)
(struct field-info (owner name init) #:transparent)
(struct instance (class fields) #:transparent)

(define unassigned-value 'unassigned)
(define no-value 'no-value)

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

(define state-find-binding
  (lambda (name state)
    (cond
      [(null? state) #f]
      [(state-lookup-in-layer name (state-current-layer state))
       => (lambda (binding) binding)]
      [else (state-find-binding name (state-enclosing-layers state))])))

(define state-lookup-binding
  (lambda (name state)
    (let ([binding (state-find-binding name state)])
      (if binding
          binding
          (error 'M_state "name used before declaration: ~a" name)))))

(define state-find-cell
  (lambda (name state)
    (let ([binding (state-find-binding name state)])
      (if binding (cadr binding) #f))))

(define state-lookup-cell
  (lambda (name state)
    (cadr (state-lookup-binding name state))))

(define cell-value
  (lambda (cell name)
    (let ([value (unbox cell)])
      (if (eq? value unassigned-value)
          (error 'M_state "name used before initialization: ~a" name)
          value))))

(define state-lookup
  (lambda (name state)
    (cell-value (state-lookup-cell name state) name)))

(define state-update
  (lambda (name value state)
    (begin
      (set-box! (state-lookup-cell name state) value)
      state)))

;; ---------------------------------------------------------------------------
;; Class and object helpers

(define class-name cadr)
(define class-extends caddr)
(define class-body cadddr)

(define parent-name
  (lambda (class-statement)
    (if (null? (class-extends class-statement))
        #f
        (cadr (class-extends class-statement)))))

(define class-definition?
  (lambda (statement)
    (and (pair? statement) (eq? 'class (car statement)))))

(define function-definition?
  (lambda (statement)
    (and (pair? statement)
         (or (eq? 'function (car statement))
             (eq? 'static-function (car statement))))))

(define nested-function-definition?
  (lambda (statement)
    (and (pair? statement) (eq? 'function (car statement)))))

(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)

(define instance-field-key
  (lambda (owner name)
    (cons owner name)))

(define class-parent-closure
  (lambda (klass state)
    (if (class-closure-parent klass)
        (state-lookup (class-closure-parent klass) state)
        #f)))

(define class-find-method
  (lambda (klass name argc include-static? state)
    (let ([matches (filter (lambda (entry)
                             (and (eq? name (car entry))
                                  (= argc (length (parse-params (closure-params (cdr entry)))))))
                           (append (class-closure-methods klass)
                                   (if include-static?
                                       (class-closure-static-methods klass)
                                       '())))])
      (cond
        [(pair? matches) (cdar matches)]
        [(class-parent-closure klass state)
         => (lambda (parent) (class-find-method parent name argc include-static? state))]
        [else #f]))))

(define class-find-static-method
  (lambda (klass name argc state)
    (let ([matches (filter (lambda (entry)
                             (and (eq? name (car entry))
                                  (= argc (length (parse-params (closure-params (cdr entry)))))))
                           (class-closure-static-methods klass))])
      (cond
        [(pair? matches) (cdar matches)]
        [(class-parent-closure klass state)
         => (lambda (parent) (class-find-static-method parent name argc state))]
        [else #f]))))

(define class-find-field-info
  (lambda (klass name state)
    (let ([matches (filter (lambda (field) (eq? name (field-info-name field)))
                           (class-closure-fields klass))])
      (cond
        [(pair? matches) (car matches)]
        [(class-parent-closure klass state)
         => (lambda (parent) (class-find-field-info parent name state))]
        [else #f]))))

(define instance-field-cell
  (lambda (object start-class name state)
    (let ([field (class-find-field-info start-class name state)])
      (if field
          (let ([binding (assoc (instance-field-key (field-info-owner field)
                                                    (field-info-name field))
                                (instance-fields object))])
            (if binding
                (cdr binding)
                (error 'M_state "field storage missing: ~a" name)))
          (error 'M_state "field not found: ~a" name)))))

(define all-fields-for-class
  (lambda (klass state)
    (let ([parent (class-parent-closure klass state)])
      (append (if parent (all-fields-for-class parent state) '())
              (class-closure-fields klass)))))

(define literal-init-value
  (lambda (expression)
    (cond
      [(not expression) unassigned-value]
      [(number? expression) expression]
      [(eq? expression 'true) 'true]
      [(eq? expression 'false) 'false]
      [else unassigned-value])))

(define make-instance
  (lambda (class-name-symbol state)
    (let* ([klass (state-lookup class-name-symbol state)]
           [fields (map (lambda (field)
                          (cons (instance-field-key (field-info-owner field)
                                                    (field-info-name field))
                                (box (literal-init-value (field-info-init field)))))
                        (all-fields-for-class klass state))])
      (instance class-name-symbol fields))))

(define dot-left-is?
  (lambda (expression name)
    (and (symbol? expression) (eq? expression name))))

;; ---------------------------------------------------------------------------
;; Result helpers

(define make-result
  (lambda (value state)
    (list value state)))

(define result-value car)
(define result-state cadr)

;; ---------------------------------------------------------------------------
;; Boolean and value operations

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

;; ---------------------------------------------------------------------------
;; Location lookup

(define lookup-variable-cell
  (lambda (name state current-class current-this)
    (cond
      [(state-find-cell name state) => (lambda (cell) cell)]
      [(and current-this current-class)
       (instance-field-cell current-this (state-lookup current-class state) name state)]
      [else (error 'M_state "name used before declaration: ~a" name)])))

(define dot-location-cell
  (lambda (target member state current-class current-this return-k throw-k)
    (cond
      [(dot-left-is? target 'this)
       (if current-this
           (return-k (instance-field-cell current-this
                                          (state-lookup current-class state)
                                          member
                                          state)
                     state)
           (error 'M_state "this used outside of an instance method"))]
      [(dot-left-is? target 'super)
       (if (and current-this current-class)
           (let ([parent (class-parent-closure (state-lookup current-class state) state)])
             (if parent
                 (return-k (instance-field-cell current-this parent member state) state)
                 (error 'M_state "super used without a parent class")))
           (error 'M_state "super used outside of an instance method"))]
      [else
       (M_value target
                state current-class current-this
                (lambda (left-value left-state)
                  (if (instance? left-value)
                      (return-k (instance-field-cell left-value
                                                     (state-lookup (instance-class left-value) left-state)
                                                     member
                                                     left-state)
                                left-state)
                      (error 'M_state "dot field access requires an object")))
                throw-k)])))

(define location-cell
  (lambda (target state current-class current-this return-k throw-k)
    (cond
      [(symbol? target)
       (return-k (lookup-variable-cell target state current-class current-this) state)]
      [(and (pair? target) (eq? 'dot (car target)))
       (dot-location-cell (cadr target)
                          (caddr target)
                          state current-class current-this
                          return-k throw-k)]
      [else (error 'M_state "invalid assignment target: ~a" target)])))

;; ---------------------------------------------------------------------------
;; Function calls

(define bind-arguments
  (lambda (params args caller-state function-state current-class current-this return-k throw-k)
    (cond
      [(and (null? params) (null? args)) (return-k function-state)]
      [(or (null? params) (null? args))
       (error 'M_value "wrong number of arguments")]
      [(eq? 'reference (param-mode (car params)))
       (location-cell (car args)
                      caller-state current-class current-this
                      (lambda (cell next-caller-state)
                        (bind-arguments (cdr params)
                                        (cdr args)
                                        next-caller-state
                                        (state-add-cell (param-name (car params))
                                                        cell
                                                        function-state)
                                        current-class current-this
                                        return-k throw-k))
                      throw-k)]
      [else
       (M_value (car args)
                caller-state current-class current-this
                (lambda (value next-caller-state)
                  (bind-arguments (cdr params)
                                  (cdr args)
                                  next-caller-state
                                  (state-add (param-name (car params))
                                             value
                                             function-state)
                                  current-class current-this
                                  return-k throw-k))
                throw-k)])))

(define call-closure
  (lambda (function-value args caller-state call-this caller-class caller-this return-k throw-k)
    (let* ([function-this (if (closure-static? function-value)
                              (closure-captured-this function-value)
                              call-this)]
           [function-class (closure-owner function-value)]
           [function-state (state-enter-scope (closure-env function-value))]
           [function-state-with-this
            (if function-this
                (state-add 'this function-this function-state)
                function-state)])
      (bind-arguments (parse-params (closure-params function-value))
                      args
                      caller-state
                      function-state-with-this
                      caller-class
                      caller-this
                      (lambda (call-state)
                        (M_state-list (closure-body function-value)
                                      call-state
                                      function-class
                                      function-this
                                      (lambda (value return-state)
                                        (return-k value caller-state))
                                      (lambda (break-state)
                                        (error 'M_state "break used outside of a loop"))
                                      (lambda (continue-state)
                                        (error 'M_state "continue used outside of a loop"))
                                      (lambda (value thrown-state)
                                        (throw-k value caller-state))
                                      (lambda (normal-state)
                                        (return-k no-value caller-state))))
                      throw-k))))

(define M_value-method-call
  (lambda (target args state current-class current-this return-k throw-k)
    (let ([method-name (caddr target)]
          [receiver (cadr target)])
      (cond
        [(dot-left-is? receiver 'super)
         (if (and current-this current-class)
             (let ([parent (class-parent-closure (state-lookup current-class state) state)])
               (if parent
                   (let ([method (class-find-method parent method-name (length args) #f state)])
                     (if method
                         (call-closure method args state current-this current-class current-this return-k throw-k)
                         (error 'M_value "method not found: ~a" method-name)))
                   (error 'M_value "super used without a parent class")))
             (error 'M_value "super used outside of an instance method"))]
        [(symbol? receiver)
         (let ([maybe-class-cell (state-find-cell receiver state)])
           (if (and maybe-class-cell (class-closure? (unbox maybe-class-cell)))
               (let* ([klass (unbox maybe-class-cell)]
                      [method (class-find-static-method klass method-name (length args) state)])
                 (if method
                     (call-closure method args state #f current-class current-this return-k throw-k)
                     (error 'M_value "static method not found: ~a" method-name)))
               (M_value receiver
                        state current-class current-this
                        (lambda (receiver-value receiver-state)
                          (if (instance? receiver-value)
                              (let* ([klass (state-lookup (instance-class receiver-value) receiver-state)]
                                     [method (class-find-method klass method-name (length args) #f receiver-state)])
                                (if method
                                    (call-closure method args receiver-state receiver-value current-class current-this return-k throw-k)
                                    (error 'M_value "method not found: ~a" method-name)))
                              (error 'M_value "method call requires an object")))
                        throw-k)))]
        [else
         (M_value receiver
                  state current-class current-this
                  (lambda (receiver-value receiver-state)
                    (if (instance? receiver-value)
                        (let* ([klass (state-lookup (instance-class receiver-value) receiver-state)]
                               [method (class-find-method klass method-name (length args) #f receiver-state)])
                          (if method
                              (call-closure method args receiver-state receiver-value current-class current-this return-k throw-k)
                              (error 'M_value "method not found: ~a" method-name)))
                        (error 'M_value "method call requires an object")))
                  throw-k)]))))

(define M_value-funcall
  (lambda (expression state current-class current-this return-k throw-k)
    (let ([target (cadr expression)]
          [args (cddr expression)])
      (cond
        [(and (pair? target) (eq? 'dot (car target)))
         (M_value-method-call target args state current-class current-this return-k throw-k)]
        [(state-find-cell target state)
         => (lambda (cell)
              (let ([function-value (cell-value cell target)])
                (if (closure? function-value)
                    (call-closure function-value
                                  args
                                  state
                                  (closure-captured-this function-value)
                                  current-class
                                  current-this
                                  return-k throw-k)
                    (error 'M_value "attempted to call a non-function: ~a" target))))]
        [(and current-this current-class)
         (let* ([klass (state-lookup (instance-class current-this) state)]
                [method (class-find-method klass target (length args) #f state)])
           (if method
               (call-closure method args state current-this current-class current-this return-k throw-k)
               (error 'M_value "function or method not found: ~a" target)))]
        [else (error 'M_value "function not found: ~a" target)]))))

;; ---------------------------------------------------------------------------
;; Value operations

(define M_value
  (lambda (expression state current-class current-this return-k throw-k)
    (cond
      [(number? expression) (return-k expression state)]
      [(eq? expression 'true) (return-k 'true state)]
      [(eq? expression 'false) (return-k 'false state)]
      [(eq? expression 'this)
       (if current-this
           (return-k current-this state)
           (error 'M_value "this used outside of an instance method"))]
      [(symbol? expression)
       (return-k (cell-value (lookup-variable-cell expression state current-class current-this)
                             expression)
                 state)]
      [(and (pair? expression) (eq? 'new (car expression)))
       (return-k (make-instance (cadr expression) state) state)]
      [(and (pair? expression) (eq? '= (car expression)))
       (M_value (caddr expression)
                state current-class current-this
                (lambda (value value-state)
                  (location-cell (cadr expression)
                                 value-state current-class current-this
                                 (lambda (cell location-state)
                                   (begin
                                     (set-box! cell value)
                                     (return-k value location-state)))
                                 throw-k))
                throw-k)]
      [(and (pair? expression) (eq? 'dot (car expression)))
       (location-cell expression
                      state current-class current-this
                      (lambda (cell location-state)
                        (return-k (cell-value cell (caddr expression)) location-state))
                      throw-k)]
      [(and (pair? expression) (eq? 'funcall (car expression)))
       (M_value-funcall expression state current-class current-this return-k throw-k)]
      [(and (pair? expression) (eq? '- (car expression)) (null? (cddr expression)))
       (M_value (cadr expression)
                state current-class current-this
                (lambda (value value-state)
                  (return-k (- value) value-state))
                throw-k)]
      [(and (pair? expression) (eq? '! (car expression)))
       (M_value (cadr expression)
                state current-class current-this
                (lambda (value value-state)
                  (return-k (M_boolean-not value) value-state))
                throw-k)]
      [(pair? expression)
       (M_value (cadr expression)
                state current-class current-this
                (lambda (left left-state)
                  (M_value (caddr expression)
                           left-state current-class current-this
                           (lambda (right right-state)
                             (return-k (M_value-binary (car expression) left right)
                                       right-state))
                           throw-k))
                throw-k)]
      [else (error 'M_value "unknown expression: ~a" expression)])))

;; ---------------------------------------------------------------------------
;; Statement operations

(define M_state-declare
  (lambda (statement state current-class current-this return-k throw-k next-k)
    (if (null? (cddr statement))
        (next-k (state-add (cadr statement) unassigned-value state))
        (M_value (caddr statement)
                 state current-class current-this
                 (lambda (value value-state)
                   (next-k (state-add (cadr statement) value value-state)))
                 throw-k))))

(define M_state-assign
  (lambda (statement state current-class current-this throw-k next-k)
    (M_value statement state current-class current-this
             (lambda (value value-state) (next-k value-state))
             throw-k)))

(define M_state-return
  (lambda (statement state current-class current-this return-k throw-k)
    (M_value (cadr statement)
             state current-class current-this
             (lambda (value value-state)
               (return-k value value-state))
             throw-k)))

(define M_state-throw
  (lambda (statement state current-class current-this throw-k)
    (M_value (cadr statement)
             state current-class current-this
             (lambda (value value-state)
               (throw-k value value-state))
             throw-k)))

(define M_state-function
  (lambda (statement state current-class current-this next-k)
    (next-k (state-update (function-name statement)
                          (closure (function-params statement)
                                   (function-body statement)
                                   state
                                   current-class
                                   #t
                                   current-this)
                          state))))

(define M_state-if
  (lambda (statement state current-class current-this return-k break-k continue-k throw-k next-k)
    (M_value (cadr statement)
             state current-class current-this
             (lambda (condition condition-state)
               (cond
                 [(M_boolean-true? condition)
                  (M_state (caddr statement)
                           condition-state current-class current-this
                           return-k break-k continue-k throw-k next-k)]
                 [(null? (cdddr statement)) (next-k condition-state)]
                 [else
                  (M_state (cadddr statement)
                           condition-state current-class current-this
                           return-k break-k continue-k throw-k next-k)]))
             throw-k)))

(define M_state-while
  (lambda (statement state current-class current-this return-k break-k continue-k throw-k next-k)
    (M_value (cadr statement)
             state current-class current-this
             (lambda (condition condition-state)
               (if (M_boolean-true? condition)
                   (M_state (caddr statement)
                            condition-state current-class current-this
                            return-k
                            (lambda (break-state) (next-k break-state))
                            (lambda (continue-state)
                              (M_state-while statement continue-state current-class current-this
                                             return-k break-k continue-k throw-k next-k))
                            throw-k
                            (lambda (body-state)
                              (M_state-while statement body-state current-class current-this
                                             return-k break-k continue-k throw-k next-k)))
                   (next-k condition-state)))
             throw-k)))

(define M_state-block
  (lambda (statements state current-class current-this return-k break-k continue-k throw-k next-k)
    (let ([block-state (state-enter-scope state)])
      (M_state-list statements
                    block-state current-class current-this
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
  (lambda (finally-clause state current-class current-this return-k break-k continue-k throw-k resume-k)
    (if (finally-clause? finally-clause)
        (M_state-block (cadr finally-clause)
                       state current-class current-this
                       return-k break-k continue-k throw-k resume-k)
        (resume-k state))))

(define M_state-catch
  (lambda (catch-clause thrown-value state current-class current-this return-k break-k continue-k throw-k next-k)
    (if (catch-clause? catch-clause)
        (let* ([catch-name (caadr catch-clause)]
               [catch-state (state-add catch-name
                                       thrown-value
                                       (state-enter-scope state))])
          (M_state-list (caddr catch-clause)
                        catch-state current-class current-this
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
  (lambda (statement state current-class current-this return-k break-k continue-k throw-k next-k)
    (let ([try-body (cadr statement)]
          [catch-clause (caddr statement)]
          [finally-clause (cadddr statement)])
      (M_state-block try-body
                     state current-class current-this
                     (lambda (value return-state)
                       (M_state-finally finally-clause
                                        return-state current-class current-this
                                        return-k break-k continue-k throw-k
                                        (lambda (finally-state)
                                          (return-k value finally-state))))
                     (lambda (break-state)
                       (M_state-finally finally-clause
                                        break-state current-class current-this
                                        return-k break-k continue-k throw-k
                                        break-k))
                     (lambda (continue-state)
                       (M_state-finally finally-clause
                                        continue-state current-class current-this
                                        return-k break-k continue-k throw-k
                                        continue-k))
                     (lambda (value throw-state)
                       (if (catch-clause? catch-clause)
                           (M_state-catch catch-clause
                                          value
                                          throw-state current-class current-this
                                          (lambda (catch-value catch-return-state)
                                            (M_state-finally finally-clause
                                                             catch-return-state current-class current-this
                                                             return-k break-k continue-k throw-k
                                                             (lambda (finally-state)
                                                               (return-k catch-value finally-state))))
                                          (lambda (catch-break-state)
                                            (M_state-finally finally-clause
                                                             catch-break-state current-class current-this
                                                             return-k break-k continue-k throw-k
                                                             break-k))
                                          (lambda (catch-continue-state)
                                            (M_state-finally finally-clause
                                                             catch-continue-state current-class current-this
                                                             return-k break-k continue-k throw-k
                                                             continue-k))
                                          (lambda (catch-value catch-throw-state)
                                            (M_state-finally finally-clause
                                                             catch-throw-state current-class current-this
                                                             return-k break-k continue-k throw-k
                                                             (lambda (finally-state)
                                                               (throw-k catch-value finally-state))))
                                          (lambda (catch-normal-state)
                                            (M_state-finally finally-clause
                                                             catch-normal-state current-class current-this
                                                             return-k break-k continue-k throw-k
                                                             next-k)))
                           (M_state-finally finally-clause
                                            throw-state current-class current-this
                                            return-k break-k continue-k throw-k
                                            (lambda (finally-state)
                                              (throw-k value finally-state)))))
                     (lambda (normal-state)
                       (M_state-finally finally-clause
                                        normal-state current-class current-this
                                        return-k break-k continue-k throw-k
                                        next-k))))))

(define M_state
  (lambda (statement state current-class current-this return-k break-k continue-k throw-k next-k)
    (case (car statement)
      [(var) (M_state-declare statement state current-class current-this return-k throw-k next-k)]
      [(=) (M_state-assign statement state current-class current-this throw-k next-k)]
      [(funcall) (M_value statement state current-class current-this
                          (lambda (value value-state) (next-k value-state))
                          throw-k)]
      [(function) (M_state-function statement state current-class current-this next-k)]
      [(return) (M_state-return statement state current-class current-this return-k throw-k)]
      [(break) (break-k state)]
      [(continue) (continue-k state)]
      [(throw) (M_state-throw statement state current-class current-this throw-k)]
      [(if) (M_state-if statement state current-class current-this return-k break-k continue-k throw-k next-k)]
      [(while) (M_state-while statement state current-class current-this return-k break-k continue-k throw-k next-k)]
      [(begin) (M_state-block (cdr statement) state current-class current-this return-k break-k continue-k throw-k next-k)]
      [(try) (M_state-try statement state current-class current-this return-k break-k continue-k throw-k next-k)]
      [else (error 'M_state "unknown statement: ~a" statement)])))

(define M_state-add-function-names
  (lambda (statements state)
    (cond
      [(null? statements) state]
      [(nested-function-definition? (car statements))
       (M_state-add-function-names
        (cdr statements)
        (state-add (function-name (car statements)) unassigned-value state))]
      [else (M_state-add-function-names (cdr statements) state)])))

(define M_state-install-function-closures
  (lambda (statements state current-class current-this)
    (cond
      [(null? statements) state]
      [(nested-function-definition? (car statements))
       (M_state-install-function-closures
        (cdr statements)
        (state-update (function-name (car statements))
                      (closure (function-params (car statements))
                               (function-body (car statements))
                               state
                               current-class
                               #t
                               current-this)
                      state)
        current-class current-this)]
      [else (M_state-install-function-closures (cdr statements) state current-class current-this)])))

(define M_state-hoist-functions
  (lambda (statements state current-class current-this)
    (M_state-install-function-closures
     statements
     (M_state-add-function-names statements state)
     current-class
     current-this)))

(define M_state-list
  (lambda (statements state current-class current-this return-k break-k continue-k throw-k next-k)
    (let ([hoisted-state (M_state-hoist-functions statements state current-class current-this)])
      (letrec ([execute
                (lambda (remaining current-state)
                  (cond
                    [(null? remaining) (next-k current-state)]
                    [else
                     (M_state (car remaining)
                              current-state current-class current-this
                              return-k break-k continue-k throw-k
                              (lambda (next-state)
                                (execute (cdr remaining) next-state)))]))])
        (execute statements hoisted-state)))))

;; ---------------------------------------------------------------------------
;; Class loading and entrypoint

(define method-entry
  (lambda (statement owner global-state static?)
    (cons (function-name statement)
          (closure (function-params statement)
                   (function-body statement)
                   global-state
                   owner
                   static?
                   #f))))

(define class-field-entry
  (lambda (statement owner)
    (field-info owner
                (cadr statement)
                (if (null? (cddr statement)) #f (caddr statement)))))

(define build-class-closure
  (lambda (statement global-state)
    (let ([owner (class-name statement)]
          [body (class-body statement)])
      (class-closure
       owner
       (parent-name statement)
       (map (lambda (field) (class-field-entry field owner))
            (filter (lambda (member) (and (pair? member) (eq? 'var (car member)))) body))
       (map (lambda (method) (method-entry method owner global-state #f))
            (filter (lambda (member) (and (pair? member) (eq? 'function (car member)))) body))
       (map (lambda (method) (method-entry method owner global-state #t))
            (filter (lambda (member) (and (pair? member) (eq? 'static-function (car member)))) body))
       '()))))

(define add-class-names
  (lambda (classes state)
    (cond
      [(null? classes) state]
      [else
       (add-class-names (cdr classes)
                        (state-add (class-name (car classes)) unassigned-value state))])))

(define install-class-closures
  (lambda (classes state)
    (cond
      [(null? classes) state]
      [else
       (install-class-closures
        (cdr classes)
        (state-update (class-name (car classes))
                      (build-class-closure (car classes) state)
                      state))])))

(define M_state-classes
  (lambda (classes)
    (install-class-closures classes (add-class-names classes empty-state))))

(define call-main
  (lambda (state class-symbol)
    (let* ([klass (state-lookup class-symbol state)]
           [static-main (class-find-static-method klass 'main 0 state)])
      (if static-main
          (call-closure static-main
                        '()
                        state
                        #f
                        #f
                        #f
                        (lambda (value value-state) value)
                        (lambda (value throw-state)
                          (error 'M_state "uncaught exception: ~a" value)))
          (let* ([object (make-instance class-symbol state)]
                 [main-method (class-find-method klass 'main 0 #f state)])
            (if main-method
                (call-closure main-method
                              '()
                              state
                              object
                              class-symbol
                              object
                              (lambda (value value-state) value)
                              (lambda (value throw-state)
                                (error 'M_state "uncaught exception: ~a" value)))
                (error 'M_state "main method not found in class: ~a" class-symbol)))))))

;; ---------------------------------------------------------------------------
;; Public entry points

(define interpret-tree
  (lambda (syntax-tree classname)
    (call-main (M_state-classes syntax-tree) classname)))

(define interpret
  (lambda (filename classname)
    (interpret-tree (parser filename) (string->symbol classname))))

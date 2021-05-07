#lang racket

;; starter code for A7, including:
;; 1. a parser that works with the A7 grammar:
(require (only-in (file "parse.rkt") parse))

;; 2. A6 solution code (everything below):

(provide eval repl)

(module+ test (require rackunit))

(define (eval-program program-expr env)
  ;; program     := exprList
  (last (eval-exprList (second program-expr) env)))

(define (eval-exprList exprList-expr env)
  ;; exprList    := expr optExprList
  (let* ([expr-expr (second exprList-expr)]
         [expr-tag (first (second expr-expr))]
         [optExprList-expr (third exprList-expr)])
    (if (equal? expr-tag 'define)
      ;; define      := DEFINE NAME expr
      (let* ([define-expr (second expr-expr)]
             [name (second (third define-expr))]
             [value-expr (fourth define-expr)]
             [new-env (add-binding env name (eval-expr value-expr env))])
        (eval-optExprList (lookup-name new-env name)
                          optExprList-expr
                          new-env))
      ;; normal stuff
      (eval-optExprList (eval-expr expr-expr env)
                        optExprList-expr
                        env))))

(define (eval-optExprList value optExprList-expr env)
  ;; optExprList := ɛ | exprList
  (cons value (if (empty? (rest optExprList-expr))
                null
                (eval-exprList (second optExprList-expr) env))))

(define (eval-expr expr-expr env)
  ;; expr        := atom | invocation | let | define | lambda | class | new | send | super | set
  (let* ([expr-to-eval (second expr-expr)]
         [tag (first expr-to-eval)]
         [evaluator (case tag
                      [(class) eval-class]
                      [(new) eval-new]
                      [(send) eval-send]
                      [(super) eval-super]
                      [(set) eval-set]
                      [(atom) eval-atom]
                      [(invocation) eval-invocation]
                      [(let) eval-let]
                      ;; define case is handled in `eval-exprList`
                      [(lambda) eval-lambda])])
    (evaluator expr-to-eval env)))

(define (eval-atom atom-expr env)
  ;; atom        := NAME | STRING | number
  (let* ([name-string-number (second atom-expr)]
         [tag (first name-string-number)]
         [evaluator (case tag
                      [(NAME) eval-name]
                      [(STRING) eval-string]
                      [(number) eval-number])])
    (evaluator name-string-number env)))

(define (eval-name name-expr env)
  ;; + - * / string-append string<? string=? not = <
  (case (second name-expr)
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(string-append) string-append]
    [(string<?) string<?]
    [(string=?) string=?]
    [(not) not]
    [(=) =]
    [(<) <]
    [else (lookup-name env (second name-expr))]))

(define (eval-string string-expr env) (second string-expr))
(define (eval-number number-expr env)
  ;; number      := INT | FLOAT
  (second (second number-expr)))

(define (eval-let let-expr env)
  ;; let         := LET OPAREN NAME expr CPAREN expr
  (let* ([name (second (fourth let-expr))]
         [value-expr (fifth let-expr)]
         [body-expr (seventh let-expr)])
    (eval-expr body-expr
               (add-binding env name (eval-expr value-expr env)))))

(define (eval-lambda lambda-expr env)
  ;; lambda      := LAMBDA OPAREN NAME CPAREN expr
  (let* ([name (second (fourth lambda-expr))]
         [body-expr (sixth lambda-expr)])
    (lambda (value)
      (eval-expr body-expr
                 (add-binding env name value)))))

(define (eval-invocation invocation-expr env)
  ;; invocation  := OPAREN exprList CPAREN
  (let* ([exprList-expr (third invocation-expr)]
         [rator-expr (second (second exprList-expr))]
         [values (eval-exprList exprList-expr env)]
         [rator (first values)]
         [rands (rest values)])
    (apply rator rands)))

(define (repl)
  (parameterize ([current-read-interaction (lambda (_ in)
                                             (read-line in))]
                 [current-eval (lambda (e)
                                 (when (non-empty-string? (cdr e))
                                   (eval (cdr e))))])
    (read-eval-print-loop)))

;;;
;;; the top-level evaluation function
;;;

(define (eval code)
  (eval-begin (parse code)
              (new-environment-with-object%)))

;;;
;;; evaluation functions
;;;

(define (eval-begin begin-expr env)
  ;; (begin expr+)
  (let ([first-expr (second begin-expr)]
        [other-exprs (rest (rest begin-expr))])
    (if (empty? other-exprs)
        (eval-expr first-expr env)
        (begin
          (eval-expr first-expr env)
          (eval-begin (cons 'begin other-exprs)
                      env)))))



(define (eval-define define-expr env)
  ;; (define name value-expr)
  (let* ([name (second define-expr)]
         [value-expr (third define-expr)]
         [value (eval-expr value-expr env)])
    (add-binding env name value)))

(define (eval-set! define-expr env)
  ;; (set! name value-expr)
  (let* ([name (second define-expr)]
         [value-expr (third define-expr)]
         [value (eval-expr value-expr env)])
    (update-binding env name value)))

;; We represent a Class internally as a list with 4 items:
;; 1. superclass
;; 2. field-names
;; 3. method-table
;; 4. defining-environment

;; The method table is a hash-table whose keys are method names (as symbols) and
;; whose values are method info tuples, which are lists containing two items:
;; 1. the list of parameter names of the method
;; 2. the body of the method

(define (eval-class class-expr env)
  ;; (class superclass-name ( field-name* ) method*)
  ;; method: (<method-name> ( param-name* ) expr+)
  (let* ([superclass-name (second class-expr)]
         [superclass (lookup-name env superclass-name)]
         [field-names (third class-expr)]
         [method-exprs (rest (rest (rest class-expr)))]
         [method-table (make-hash
                        (for/list ([method-expr method-exprs])
                          (let ([method-name (first method-expr)]
                                [method-params (second method-expr)]
                                [method-body (rest (rest method-expr))])
                            (cons method-name
                                  (list method-params method-body)))))])
    (list superclass
          field-names
          method-table
          (hash-copy env))))

;; We represent an Object internally as a list with 2 items:
;; 1. the class it was created from
;; 2. the defining environment, extended to include bindings for each of the
;;    fields

(define (eval-new new-expr env)
  ;; (new class-name arg*)
  (let* ([class-name (second new-expr)]
         [class (lookup-name env class-name)]
         [rand-exprs (rest (rest new-expr))]
         [field-names (collect-field-names class)]
         [class-env (fourth class)]
         [object-env (add-field-bindings (hash-copy class-env) field-names)]
         [object (list class object-env)])
    (call-method object 'initialize rand-exprs env)
    object))

(define (eval-send send-expr env)
  ;; (send object-name method-name arg*)
  (let* ([object-name (second send-expr)]
         [method-name (third send-expr)]
         [rand-exprs (rest (rest (rest send-expr)))]
         [object (lookup-name env object-name)])
    (call-method object method-name rand-exprs env)))

(define (eval-super super-expr env)
  ;; (super arg*)
  (let* ([object (lookup-name env 'this)]
         [super-method-info (lookup-name env 'super)]
         [method-name (first super-method-info)]
         [relative-class (second super-method-info)]
         [method-class (find-method-class-or-fail relative-class method-name)]
         [rand-exprs (rest super-expr)])
    (call-method2 object method-class method-name rand-exprs env)))

;; We represent an Interface internally as a list of method-info tuples, each of
;; which is a list with 2 items:
;; 1. the method name
;; 2. the method arity

(define (eval-interface interface-expr env)
  ;; (interface (method1 param*) (method2 param*) ...)
  (let ([method-exprs (rest interface-expr)])
    (for/list ([method-expr method-exprs])
      (let* ([method-name (first method-expr)]
             [method-arity (length (rest method-expr))]
             [method-info (list method-name method-arity)])
        method-info))))

(define (eval-class2 class2-expr env)
  ;; (class2 superclass-name ( interface-name* ) ( field-name* ) method*)
  (let* ([superclass-name (second class2-expr)]
         [interface-names (third class2-expr)]
         [field-names (fourth class2-expr)]
         [method-exprs (rest (rest (rest (rest class2-expr))))]
         [class-expr (cons 'class
                           (cons superclass-name
                                 (cons field-names
                                       method-exprs)))]
         [class (eval-class class-expr env)]
         [interfaces (for/list ([interface-name interface-names])
                       (lookup-name env interface-name))])
    (check-interfaces class interfaces)
    class))

;;;
;;; method calls
;;;

(define (call-method object method-name rand-exprs rand-env)
  (let* ([class (first object)]
         [method-class (find-method-class-or-fail class method-name)])
    (call-method2 object method-class method-name rand-exprs rand-env)))

(define (call-method2 object method-class method-name rand-exprs rand-env)
  (let* ([method-table (third method-class)]
         [method-info (hash-ref method-table method-name)]
         [method-params (first method-info)]
         [method-body (second method-info)]
         [method-body-as-begin-expr (cons 'begin method-body)]
         [args (eval-rand-exprs rand-exprs rand-env)]
         [object-env (second object)]
         [env-with-this (add-binding object-env 'this object)]
         [object-env-with-parameter-bindings (add-bindings env-with-this
                                                           method-params
                                                           args)]
         [super-method-class (first method-class)]
         [method-env (add-binding object-env-with-parameter-bindings
                                  'super
                                  (list method-name super-method-class))])
    (eval-begin method-body-as-begin-expr method-env)))

(define (find-method-class-or-fail class method-name)
  (or (find-method class method-name)
      (error (~a "Method not found: " method-name))))

(define (find-method class method-name)
  (let ([superclass (first class)]
        [method-table (third class)])
    (if (not superclass)
        #f
        (if (hash-has-key? method-table method-name)
            class
            (find-method superclass method-name)))))

;;;
;;; other OOP helper functions
;;;

(define (new-environment-with-object%)
  (let* ([new-env (new-environment)]
         [object% (list
                   #f          ; 1. superclass
                   '()         ; 2. field-names
                   (make-hash) ; 3. method-table
                   new-env     ; 4. defining-environment
                   )])
    (add-binding new-env 'object% object%)))

(define (collect-field-names class)
  (let ([superclass (first class)]
        [field-names (second class)])
    (if (not superclass)
        field-names
        (remove-duplicates (append field-names
                                   (collect-field-names superclass))))))

(define (check-interfaces class interfaces)
  (for* ([interface interfaces]
         [method-info interface])
    (let ([method-name (first method-info)]
          [method-arity (second method-info)])
      (check-method-existence-and-arity class method-name method-arity))))

(define (check-method-existence-and-arity class method-name method-arity)
  (let* ([method-class (find-method-class-or-fail class method-name)]
         [method-table (third method-class)]
         [method-info (hash-ref method-table method-name)]
         [method-params (first method-info)])
    (unless (= method-arity (length method-params))
      (error (~a "Invalid arity for implemented method " method-name)))))

;;;;
;;;; evaluation helper functions
;;;;

(define (invoke-builtin-function invocation-expr env)
  (let* ([eval-arg-expr (lambda (expr) (eval-expr expr env))]
         [rator (first invocation-expr)]
         [fn (if (equal? '+ rator) + (if (equal? 'add1 rator) add1 string-append))]
         [arg-exprs (rest invocation-expr)]
         [args (map eval-arg-expr arg-exprs)])
    (apply fn args)))

(define (eval-rand-exprs rand-exprs env)
  (let ([eval-rand-expr (lambda (expr) (eval-expr expr env))])
    (map eval-rand-expr rand-exprs)))

;;;
;;; the environment implementation and environment helper functions
;;;

(define new-environment make-hash)

(define (add-binding env name value)
  (hash-set! env name value)
  env)

(define lookup-name hash-ref)

(define (update-binding env name value)
  (if (hash-has-key? env name)
      (begin
        (hash-set! env name value)
        env)
      (error (~a "Cannot update nonexistent name " name))))

(define (add-bindings env names values)
  (if (empty? names)
      env
      (add-bindings (add-binding env (first names) (first values))
                    (rest names) (rest values))))

(define (add-field-bindings env field-names)
  (if (empty? field-names)
      env
      (add-field-bindings (add-binding env (first field-names) (void))
                          (rest field-names))))

(module+ test
  (check-equal? (eval "define Box
  class Object (x) {
    initialize(x0) {
      set x x0
    }
    reset(new-x) {
      set x new-x
    }
    fetch() {
      x
    }
  }

define NumberBox
  class Box (x) {
    initialize(x0) {
      super(x0)
    }
    add1() {
      set x (+ x 1)
    }
    sub1() {
      set x (- x 1)
    }
  }

define AdderBox
  class NumberBox (x) {
    initialize(x0) {
      super(x0)
    }
    add(y) {
      set x (+ x y)
    }
    sub(y) {
      send this add((- y))
    }
  }

define StatelessAdder
  class Object () {
    add2(x y) {
      let (box1 new AdderBox(x))
       send box1 add(y)
    }
  }

define adder new StatelessAdder()
send adder add2(3 39)") 42))


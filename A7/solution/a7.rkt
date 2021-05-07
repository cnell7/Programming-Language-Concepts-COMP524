#lang racket

(provide eval repl)

(require "parse.rkt")

(module+ test (require rackunit))

;; environment interface
(define new-environment make-hash)
(define new-env-from hash-copy)
(define (add-binding env name value)
  (hash-set! env name value)
  env)
(define lookup-name hash-ref)
(define binding-exists? hash-has-key?)
(define (update-binding env name value)
  (if (binding-exists? env name)
    (add-binding env name value)
    (error "Cannot update nonexistent name " name)))
(define (add-bindings env names values)
  (foldl (λ (n v e) (add-binding e n v))
         env names values))

;; poor man's structs

;; class representation
(define (class superclass field-names method-table defining-env)
  (list superclass field-names method-table defining-env))
(define class-superclass first)
(define class-field-names second)
(define class-method-table third)
(define class-defining-env fourth)
(define (class-rec-field-names class)
  (let ([superclass (class-superclass class)]
        [field-names (class-field-names class)])
    (if superclass
      (remove-duplicates (append field-names (class-rec-field-names superclass)))
      field-names)))

;; method representation
(define (method params body) (list params body))
(define method-params first)
(define method-body second)

;; object representation
(define (object class env) (list class env))
(define object-class first)
(define object-env second)

(define (base-object)
  (class #f null
    `#hash((initialize . (,null (exprList (expr (atom (number (INT 0)))) (optExprList)))))
    (new-environment)))

(define (base-env)
  (add-binding (new-environment) 'Object (base-object)))

(define (eval code-string)
  (eval-program (parse code-string) (base-env)))

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
  ;; expr := atom | invocation | let | define | lambda | class | new | send | super | set
  (let* ([expr-to-eval (second expr-expr)]
         [tag (first expr-to-eval)]
         [evaluator (case tag
                      [(atom) eval-atom]
                      [(invocation) eval-invocation]
                      [(let) eval-let]
                      [(set) eval-set]
                      [(class) eval-class]
                      [(new) eval-new]
                      [(send) eval-send]
                      [(super) eval-super]
                      [(interface) eval-interface]
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

(define (eval-set set-expr env)
  ;; set := SET NAME expr
  (let* ([name (second (third set-expr))]
         [value-expr (fourth set-expr)]
         [value (eval-expr value-expr env)])
    (update-binding env name value)
    value))

(define (opt*list->list opt*list-expr)
  (if (empty? (rest opt*list-expr))
    null
    (*list->list (second opt*list-expr))))
(define (*list->list *list-expr)
  (cons (second *list-expr)
        (opt*list->list (third *list-expr))))

(define (eval-class class-expr env)
  ;; class  := CLASS NAME optInterfaceDeclaration OPAREN optNameList CPAREN OBRACE optMethodList CBRACE
  ;; superclass field-names method-table defining-env
  ;; optInterfaceDeclaration := ε | OBRACE nameList CBRACE
  (let* ([superclass-name (second (third class-expr))]
         [superclass (lookup-name env superclass-name)]
         [field-name-opt-list (sixth class-expr)]
         [field-name-list (opt*list->list field-name-opt-list)]
         [field-names (map second field-name-list)]
         [method-opt-list (ninth class-expr)]
         [method-list (opt*list->list method-opt-list)]
         [method-table (make-method-table method-list)]
         [defining-env (new-env-from env)]
         [class (class superclass field-names method-table defining-env)]
         [opt-interface-decl (fourth class-expr)])
    (if (empty? (rest opt-interface-decl))
      ;; no interfaces
      class
      ;; interfaces
      (let* ([interface-names-list (third opt-interface-decl)]
             [interface-name-exprs (*list->list interface-names-list)]
             [interface-names (map second interface-name-exprs)]
             [interfaces (map (curry lookup-name env) interface-names)])
        (map (curry check-interface class) interfaces)
        class))))

(define (check-interface class interface)
  ;; interface := method-decl list
  (map (curry check-method class) interface))

(define (check-method class method-decl)
  (let* ([name (method-decl-name method-decl)]
         [arity (method-decl-arity method-decl)]

         [method-class (find-method-class-or-fail class name)]
         [method-table (class-method-table method-class)]
         [method (get-method method-table name)]
         [params (method-params method)])
    (unless (= arity (length params))
      (error "Interface implementation: arity " (length params)
             " of name " name
             " doesn't match expected " arity))))

(define (make-method-table method-list)
  ;; method := NAME OPAREN optNameList CPAREN OBRACE exprList CBRACE
  (let ([method-table-entry
          (λ (method-expr)
            (let* ([method-name (second (second method-expr))]
                   [params-opt-list (fourth method-expr)]
                   [params-list (opt*list->list params-opt-list)]
                   [params (map second params-list)]
                   [body-expr (seventh method-expr)])
              (cons method-name (method params body-expr))))])
    (make-hash (map method-table-entry method-list))))
(define method-defined? hash-has-key?)
(define get-method hash-ref)

(define (eval-new new-expr env)
  ;; new := NEW NAME OPAREN optExprList CPAREN
  (let* ([class-name (second (third new-expr))]
         [class (lookup-name env class-name)]
         [field-names (class-rec-field-names class)]
         [object-env (add-bindings (new-env-from (class-defining-env class))
                                   field-names
                                   (map (const (void)) field-names))]
         [object (object class object-env)]

         [rand-exprs-list (fifth new-expr)]
         [rand-exprs (opt*list->list rand-exprs-list)])
    (call-method object 'initialize rand-exprs env)
    object))

(define (eval-send send-expr env)
  ;; send  := SEND NAME NAME OPAREN optExprList CPAREN
  (let* ([object-name (second (third send-expr))]
         [object (lookup-name env object-name)]
         [method-name (second (fourth send-expr))]
         [rand-exprs-list (sixth send-expr)]
         [rand-exprs (opt*list->list rand-exprs-list)])
    (call-method object method-name rand-exprs env)))

(define (eval-super super-expr env)
  ;; super := SUPER OPAREN optExprList CPAREN
  (let* ([object (lookup-name env 'this)]
         [super-method (lookup-name env 'super)]
         [name (super-method-name super-method)]
         [relative-class (super-method-class super-method)]
         [method-class (find-method-class-or-fail relative-class name)]
         [rand-exprs-list (fourth super-expr)]
         [rand-exprs (opt*list->list rand-exprs-list)])
    (call-method-directly object method-class name rand-exprs env)))

(define (eval-interface interface-expr env)
  ;; interface := INTERFACE OBRACE declarationList CBRACE
  (let* ([make-method-decl
           ;; declaration := NAME OPAREN optNameList CPAREN
           (λ (decl-expr)
             (let* ([name (second (second decl-expr))]
                    [names-opt-list (fourth decl-expr)]
                    [names (opt*list->list names-opt-list)]
                    [arity (length names)])
               (method-decl name arity)))]

         [declaration-exprs-list (fourth interface-expr)]
         [declaration-exprs (*list->list declaration-exprs-list)])
    (map make-method-decl declaration-exprs)))
(define (method-decl name arity) (list name arity))
(define method-decl-name first)
(define method-decl-arity second)

(define (find-method-class-or-fail class method-name)
  (or (find-method class method-name)
      (error "Method not found: " method-name)))

(define (find-method class method-name)
  (and class
       (let ([superclass (class-superclass class)]
             [method-table (class-method-table class)])
         (if (method-defined? method-table method-name)
           class
           (find-method superclass method-name)))))

(define (call-method object method-name rand-exprs rand-env)
  (let* ([class (object-class object)]
         [method-class (find-method-class-or-fail class method-name)])
    (call-method-directly object method-class method-name rand-exprs rand-env)))

(define (call-method-directly object method-class method-name rand-exprs rand-env)
  (let* ([method-table (class-method-table method-class)]
         [method (get-method method-table method-name)]
         [params (method-params method)]
         [body-expr (method-body method)]

         [args (map (λ (e) (eval-expr e rand-env)) rand-exprs)]

         [object-env (object-env object)]
         [env-with-this (add-binding object-env 'this object)]
         [env-with-args (add-bindings env-with-this params args)]

         [superclass (class-superclass method-class)]
         [env-with-super (add-binding env-with-args 'super (super-method method-name superclass))])
    (last (eval-exprList body-expr env-with-args))))

(define (super-method name class) (list name class))
(define super-method-name first)
(define super-method-class second)

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

(module+ test
  (check-equal? (eval "7") 7)
  (check-equal? (eval "7.7") 7.7)
  (check-equal? (eval "\"a string\"") "a string")
  (check-exn exn:fail? (thunk (eval "foo")))
  (check-exn exn:fail? (thunk (eval "(list)")))
  (check-equal? (eval "7 8") 8)
  (check-equal? (eval "(+)") 0)
  (check-equal? (eval "(+ 7)") 7)
  (check-equal? (eval "(+ 7 8)") 15)
  (check-equal? (eval "(+ 7 8 15.0)") 30.0)
  (check-exn exn:fail? (thunk (eval "(-)")))
  (check-equal? (eval "(- 7)") -7)
  (check-equal? (eval "(- 7 -8)") 15)
  (check-equal? (eval "(- 7 -8 15.0)") 0.0)
  (check-equal? (eval "(*)") 1)
  (check-equal? (eval "(* 7)") 7)
  (check-equal? (eval "(* 7 8)") 56)
  (check-equal? (eval "(* 7 8 15.0)") 840.0)
  (check-exn exn:fail? (thunk (eval "(/)")))
  (check-exn exn:fail? (thunk (eval "(/ 1 0)")))
  (check-equal? (eval "(/ 7)") 1/7)
  (check-equal? (eval "(/ 7 8)") 7/8)
  (check-equal? (eval "(/ 7 8 15.0)") (/ 7 8 15.0))
  (check-equal? (eval "(+ 7 (- 8 1))") (+ 7 (- 8 1)))
  (check-equal? (eval "(+ 7 (+ 8 1)) (+ 7 (- 8 1))") (+ 7 (- 8 1)))
  (check-equal? (eval "(string-append)") "")
  (check-equal? (eval "(string-append \"abc\")") "abc")
  (check-equal? (eval "(string-append \"abc\" \"def\")") (string-append "abc" "def"))
  (check-equal? (eval "(string-append \"abc\" \"def\" \"ghi\")") (string-append "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(string<?)")))
  (check-equal? (eval "(string<? \"abc\")") #t)
  (check-equal? (eval "(string<? \"abc\" \"def\")") (string<? "abc" "def"))
  (check-equal? (eval "(string<? \"def\" \"abc\")") (string<? "def" "abc"))
  (check-equal? (eval "(string<? \"abc\" \"def\" \"ghi\")") (string<? "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(string=?)")))
  (check-equal? (eval "(string=? \"abc\")") #t)
  (check-equal? (eval "(string=? \"abc\" \"def\")") (string=? "abc" "def"))
  (check-equal? (eval "(string=? \"abc\" \"abc\")") (string=? "abc" "abc"))
  (check-equal? (eval "(string=? \"abc\" \"def\" \"ghi\")") (string=? "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(not)")))
  (check-exn exn:fail? (thunk (eval "(not 1 2)")))
  (check-equal? (eval "(not 1)") #f)
  (check-equal? (eval "(not 1.0)") #f)
  (check-equal? (eval "(not (= 0 1))") #t)
  (check-exn exn:fail? (thunk (eval "(=)")))
  (check-equal? (eval "(= 1)") #t)
  (check-equal? (eval "(= 0 1)") #f)
  (check-equal? (eval "(= 1.0 1)") #t)
  (check-equal? (eval "(= 0 1 2)") #f)
  (check-equal? (eval "(= (+ 1 0) 1 (- 2 1))") #t)
  (check-exn exn:fail? (thunk (eval "(<)")))
  (check-equal? (eval "(< 1)") #t)
  (check-equal? (eval "(< 1 0)") #f)
  (check-equal? (eval "(< 0 1)") #t)
  (check-equal? (eval "(< 0 1 2)") #t)
  (check-equal? (eval "(< 0 1 2.0)") #t)
  (check-equal? (eval "(< (+ 1 0) 1 (- 2 1))") #f)

  (check-equal? (eval "(+ 7 (+ 8 1)) (+ 7 (- 8 1)) (< 1 1 1) \"end\"") "end")

  (check-equal? (eval "let (x (+ 1 2)) (+ x 3)") 6)
  ;; these are allowed to be implementation defined, so I just return a
  ;; procedure; there are other ways to proceed
  #;(check-equal? (eval "lambda (x) (* x x)")
                (list 'x
                      '(expr
                         (invocation
                           (OPAREN #f)
                           (exprList
                             (expr (atom (NAME *)))
                             (optExprList
                               (exprList
                                 (expr (atom (NAME x)))
                                 (optExprList (exprList (expr (atom (NAME x))) (optExprList))))))
                           (CPAREN #f)))
                      (new-environment)))
  #;(check-equal? (eval "let (y 1) lambda (x) (* y x)")
                (list 'x
                      '(expr
                         (invocation
                           (OPAREN #f)
                           (exprList
                             (expr (atom (NAME *)))
                             (optExprList
                               (exprList
                                 (expr (atom (NAME y)))
                                 (optExprList (exprList (expr (atom (NAME x))) (optExprList))))))
                           (CPAREN #f)))
                      #hash((y . 1))))
  (check-equal? (eval "(lambda (x) (* x x) 7)") 49)
  (check-equal? (eval "let (square lambda (x) (* x x)) (square 7)") 49)
  (check-equal? (eval "define foo 3") 3)
  (check-equal? (eval "define foo 3 foo") 3)
  (check-equal? (eval "define foo 3 4") 4)
  (check-equal? (eval "define foo 3 (+ 1 foo)") 4)
  (check-equal? (eval "define foo (/ 8 2) let (x (+ 1 2)) (+ x foo)") 7)
  (check-equal? (eval "define foo 3 set foo 4 foo") 4)
  (check-equal? (eval "define foo 3 set foo 4") 4)
  (check-exn exn:fail? (thunk (eval "set foo 4")))
  (check-equal? (eval "define Counter class Object (x) { initialize() { set x (- 1) } tick() { set x (+ 1 x) x } } 7")
                7)
  (check-equal? (eval "new Object() 1") 1)
  (check-equal? (eval "define foo new Object() send foo initialize()") 0))

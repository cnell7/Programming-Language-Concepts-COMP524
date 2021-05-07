#lang racket

(provide parse)

(require (only-in (file "lex.rkt")
                  lex))

;; Grammar:
;;
;; program ::= exprList
;; exprList ::= expr optExprList
;; optExprList ::= ɛ | exprList
;; expr ::= atom | invocation
;; atom ::= NAME | STRING | number
;; number ::= INT | FLOAT
;; invocation ::= OPAREN exprList CPAREN

(module+ test
  (require (only-in rackunit
                    check-equal?
                    check-exn
                    check-not-exn)))

(define tokens (make-parameter null))

(define DEBUG #f)
(define (trace label)
  (when DEBUG
    (writeln (~a label " "
                 (if (empty? (tokens))
                     'null
                     (first (tokens)))))))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(module+ test
  (check-equal? (parameterize ([tokens null])
                  (check 'DEF))
                #f)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'DEF))
                #t)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'FUN))
                #f))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))
    (when DEBUG
      (displayln (~a "(consume '" token ")")))
    token))

(module+ test
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens null])
                 (consume 'DEF))))
  (check-not-exn (lambda ()
                   (parameterize ([tokens (list (list 'DEF #f))])
                     (consume 'DEF))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (consume 'FUN)))))

(define (parse-number)
  ;; number ::= INT | FLOAT
  (list 'number
        (if (check 'INT)
            (consume 'INT)
            (consume 'FLOAT))))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-number))
                (list 'number (list 'INT 7)))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-number))
                (list 'number (list 'FLOAT 7.7)))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-number)))))

(define (parse-atom)
  ;; atom ::= NAME | STRING | number
  (list 'atom
        (cond
          [(check 'NAME) (consume 'NAME)]
          [(check 'STRING) (consume 'STRING)]
          [else (parse-number)])))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'NAME 'foo))])
                  (parse-atom))
                (list 'atom (list 'NAME 'foo)))
  (check-equal? (parameterize ([tokens (list (list 'STRING "foo"))])
                  (parse-atom))
                (list 'atom (list 'STRING "foo")))
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'INT 7))))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'FLOAT 7.7))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-atom)))))

(define (parse-invocation)
  ;; invocation ::= OPAREN exprList CPAREN
  (list 'invocation
        (consume 'OPAREN)
        (parse-expr-list)
        (consume 'CPAREN)))

(define (invocation-pending?)
  (check 'OPAREN))

(define (parse-expr)
  ;; expr ::= atom | invocation | let | define | lambda | class | new | send | super | set
  (list 'expr
        (cond
          [(atom-pending?) (parse-atom)]
          [(let-pending?) (parse-let)]
          [(define-pending?) (parse-define)]
          [(lambda-pending?) (parse-lambda)]
          [(set-pending?) (parse-set)]
          [(class-pending?) (parse-class)]
          [(new-pending?) (parse-new)]
          [(send-pending?) (parse-send)]
          [(super-pending?) (parse-super)]
          [(interface-pending?) (parse-interface)]
          [else  (parse-invocation)])))

(define (let-pending?) (check 'LET))
(define (define-pending?) (check 'DEFINE))
(define (lambda-pending?) (check 'LAMBDA))
(define (set-pending?) (check 'SET))
(define (class-pending?) (check 'CLASS))
(define (new-pending?) (check 'NEW))
(define (send-pending?) (check 'SEND))
(define (super-pending?) (check 'SUPER))
(define (interface-pending?) (check 'INTERFACE))

(define (parse-let)
  (list 'let
        (consume 'LET)
        (consume 'OPAREN)
        (consume 'NAME)
        (parse-expr)
        (consume 'CPAREN)
        (parse-expr)))

(define (parse-define)
  (list 'define
        (consume 'DEFINE)
        (consume 'NAME)
        (parse-expr)))

(define (parse-lambda)
  (list 'lambda
        (consume 'LAMBDA)
        (consume 'OPAREN)
        (consume 'NAME)
        (consume 'CPAREN)
        (parse-expr)))

(define (parse-set)
  (list 'set
        (consume 'SET)
        (consume 'NAME)
        (parse-expr)))

(define (parse-class)
  (list 'class
        (consume 'CLASS)
        (consume 'NAME)
        (parse-opt-interface-declaration)
        (consume 'OPAREN)
        (parse-opt-name-list)
        (consume 'CPAREN)
        (consume 'OBRACE)
        (parse-opt-method-list)
        (consume 'CBRACE)))

(define (interface-declaration-pending?) (check 'OBRACE))
(define (parse-opt-interface-declaration)
  (if (interface-declaration-pending?)
    (list 'optInterfaceDeclaration
          (consume 'OBRACE)
          (parse-name-list)
          (consume 'CBRACE))
    (list 'optInterfaceDeclaration)))

(define (parse-opt-name-list)
  (if (name-list-pending?)
    (list 'optNameList (parse-name-list))
    (list 'optNameList)))

(define (name-list-pending?) (check 'NAME))

(define (parse-name-list)
  (list 'nameList
        (consume 'NAME)
        (parse-opt-name-list)))

(define (parse-opt-method-list)
  (if (method-list-pending?)
    (list 'optMethodList (parse-method-list))
    (list 'optMethodList)))

(define (method-list-pending?) (method-pending?))
(define (method-pending?) (check 'NAME))

(define (parse-method-list)
  (list 'methodList
        (parse-method)
        (parse-opt-method-list)))

(define (parse-method)
  (list 'method
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-name-list)
        (consume 'CPAREN)
        (consume 'OBRACE)
        (parse-expr-list)
        (consume 'CBRACE)))

(define (parse-new)
  (list 'new
        (consume 'NEW)
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-send)
  (list 'send
        (consume 'SEND)
        (consume 'NAME)
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-super)
  (list 'super
        (consume 'SUPER)
        (consume 'OPAREN)
        (parse-opt-expr-list)
        (consume 'CPAREN)))

(define (parse-interface)
  (list 'interface
        (consume 'INTERFACE)
        (consume 'OBRACE)
        (parse-declaration-list)
        (consume 'CBRACE)))

(define (parse-declaration-list)
  (list 'declarationList
        (parse-declaration)
        (parse-opt-declaration-list)))

(define (declaration-list-pending?) (declaration-pending?))
(define (declaration-pending?) (check 'NAME))

(define (parse-opt-declaration-list)
  (if (declaration-list-pending?)
    (list 'optDeclarationList (parse-declaration-list))
    (list 'optDeclarationList)))

(define (parse-declaration)
  (list 'declaration
        (consume 'NAME)
        (consume 'OPAREN)
        (parse-opt-name-list)
        (consume 'CPAREN)))

(define (expr-pending?)
  (or (atom-pending?)
      (let-pending?)
      (define-pending?)
      (lambda-pending?)
      (set-pending?)
      (class-pending?)
      (new-pending?)
      (send-pending?)
      (super-pending?)
      (interface-pending?)
      (invocation-pending?)))

(define (parse-expr-list)
  ;; exprList ::= expr optExprList
  (list 'exprList
        (parse-expr)
        (parse-opt-expr-list)))

(define (expr-list-pending?)
  (expr-pending?))

(define (parse-opt-expr-list)
  ;; optExprList ::= ɛ | exprList
  (if (expr-list-pending?)
      (list 'optExprList (parse-expr-list))
      (list 'optExprList)))

(define (parse-program)
  ;; program ::= exprList
  (list 'program
        (parse-expr-list)))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(module+ test ;; smaller integration tests
  (check-equal? (parse "7")
                '(program
                   (exprList
                     (expr
                       (atom
                         (number
                           (INT 7))))
                     (optExprList))))

  (check-equal? (parse "7.7")
                '(program
                   (exprList
                     (expr
                       (atom
                         (number
                           (FLOAT 7.7))))
                     (optExprList))))

  (check-equal? (parse "\"a string\"")
                '(program
                   (exprList
                     (expr
                       (atom
                         (STRING "a string")))
                     (optExprList))))

  (check-equal? (parse "foo")
                '(program
                   (exprList
                     (expr
                       (atom
                         (NAME foo)))
                     (optExprList))))

  ;; invocation
  (check-equal? (parse "(list)")
                '(program
                   (exprList
                     (expr
                       (invocation
                         (OPAREN #f)
                         (exprList
                           (expr
                             (atom
                               (NAME list)))
                           (optExprList))
                         (CPAREN #f)))
                     (optExprList))))

  ;; concatenation
  (check-equal? (parse "7 8")
                '(program
                   (exprList
                     (expr
                       (atom
                         (number
                           (INT 7))))
                     (optExprList
                       (exprList
                         (expr
                           (atom
                             (number
                               (INT 8))))
                         (optExprList))))))

  ;; let
  (check-equal? (parse "let (x 1) (+ x 2)")
                '(program
                   (exprList
                     (expr
                       (let
                         (LET #f)
                         (OPAREN #f) (NAME x) (expr (atom (number (INT 1)))) (CPAREN #f)
                         (expr (invocation
                                 (OPAREN #f)
                                 (exprList
                                   (expr (atom (NAME +)))
                                   (optExprList
                                     (exprList
                                       (expr (atom (NAME x)))
                                       (optExprList
                                         (exprList
                                           (expr (atom (number (INT 2))))
                                           (optExprList))))))
                                 (CPAREN #f)))))
                     (optExprList))))
  ;; define
  (check-equal? (parse "define foo \"abc\"")
                '(program
                   (exprList
                     (expr
                       (define
                         (DEFINE #f) (NAME foo)
                         (expr (atom (STRING "abc")))))
                     (optExprList))))
  (check-equal? (parse "define foo \"abc\" let (x 1) x")
                '(program
                   (exprList
                     (expr
                       (define
                         (DEFINE #f) (NAME foo)
                         (expr (atom (STRING "abc")))))
                     (optExprList
                       (exprList
                         (expr
                           (let
                             (LET #f)
                             (OPAREN #f) (NAME x) (expr (atom (number (INT 1)))) (CPAREN #f)
                             (expr (atom (NAME x)))))
                         (optExprList))))))
  ;; lambda
  (check-equal? (parse "lambda (y) foo")
                '(program
                   (exprList
                     (expr
                       (lambda
                         (LAMBDA #f) (OPAREN #f) (NAME y) (CPAREN #f)
                         (expr (atom (NAME foo)))))
                     (optExprList))))
  ;; set
  (check-equal? (parse "set foo 7")
                '(program
                   (exprList
                     (expr (set (SET #f) (NAME foo) (expr (atom (number (INT 7))))))
                     (optExprList))))
  ;; class
  (check-equal? (parse "class Object (x) {
                         initialize () { set x -1 }
                         tick(n) { set x (+ x n) }
                       }")
                '(program
                   (exprList
                     (expr
                       (class (CLASS #f)
                         (NAME Object)
                         (optInterfaceDeclaration)
                         (OPAREN #f) (optNameList (nameList (NAME x) (optNameList))) (CPAREN #f)
                         (OBRACE #f)
                         (optMethodList
                           (methodList
                             (method
                               (NAME initialize) (OPAREN #f) (optNameList) (CPAREN #f)
                               (OBRACE #f)
                               (exprList
                                 (expr (set (SET #f) (NAME x) (expr (atom (number (INT -1))))))
                                 (optExprList))
                               (CBRACE #f))
                             (optMethodList
                               (methodList
                                 (method
                                   (NAME tick) (OPAREN #f) (optNameList (nameList (NAME n) (optNameList))) (CPAREN #f)
                                   (OBRACE #f)
                                   (exprList
                                     (expr
                                       (set (SET #f) (NAME x)
                                            (expr
                                              (invocation
                                                (OPAREN #f)
                                                (exprList
                                                  (expr (atom (NAME +)))
                                                  (optExprList
                                                    (exprList
                                                      (expr (atom (NAME x)))
                                                      (optExprList
                                                        (exprList
                                                          (expr (atom (NAME n)))
                                                          (optExprList))))))
                                                (CPAREN #f)))))
                                     (optExprList))
                                   (CBRACE #f))
                                 (optMethodList)))))
                           (CBRACE #f)))
                       (optExprList))))
  ;; new
  (check-equal? (parse "new Object()")
                '(program
                   (exprList
                     (expr (new (NEW #f) (NAME Object)
                                (OPAREN #f) (optExprList) (CPAREN #f)))
                     (optExprList))))
  ;; send
  (check-equal? (parse "send foo bar()")
                '(program
                   (exprList
                     (expr (send (SEND #f) (NAME foo) (NAME bar)
                                 (OPAREN #f) (optExprList) (CPAREN #f)))
                     (optExprList))))
  ;; super
  (check-equal? (parse "super()")
                '(program
                   (exprList
                     (expr (super (SUPER #f) (OPAREN #f) (optExprList) (CPAREN #f)))
                     (optExprList))))
  ;; interface
  (check-equal? (parse "interface { greet(name) ungreet(name) }")
                '(program
                   (exprList
                     (expr
                       (interface
                         (INTERFACE #f) (OBRACE #f)
                         (declarationList
                           (declaration
                             (NAME greet)
                             (OPAREN #f)
                             (optNameList (nameList (NAME name) (optNameList)))
                             (CPAREN #f))
                           (optDeclarationList
                             (declarationList
                               (declaration
                                 (NAME ungreet)
                                 (OPAREN #f)
                                 (optNameList (nameList (NAME name) (optNameList)))
                                 (CPAREN #f))
                               (optDeclarationList))))
                         (CBRACE #f)))
                     (optExprList))))
  ;; interface implementation
  (check-equal? (parse "class Object {Greeter Adder} (x) {
                         initialize () { set x -1 }
                         tick(n) { set x (+ x n) }
                       }")
                '(program
                   (exprList
                     (expr
                       (class (CLASS #f)
                         (NAME Object)
                         (optInterfaceDeclaration
                           (OBRACE #f)
                           (nameList (NAME Greeter) (optNameList (nameList (NAME Adder) (optNameList))))
                           (CBRACE #f))
                         (OPAREN #f) (optNameList (nameList (NAME x) (optNameList))) (CPAREN #f)
                         (OBRACE #f)
                         (optMethodList
                           (methodList
                             (method
                               (NAME initialize) (OPAREN #f) (optNameList) (CPAREN #f)
                               (OBRACE #f)
                               (exprList
                                 (expr (set (SET #f) (NAME x) (expr (atom (number (INT -1))))))
                                 (optExprList))
                               (CBRACE #f))
                             (optMethodList
                               (methodList
                                 (method
                                   (NAME tick) (OPAREN #f) (optNameList (nameList (NAME n) (optNameList))) (CPAREN #f)
                                   (OBRACE #f)
                                   (exprList
                                     (expr
                                       (set (SET #f) (NAME x)
                                            (expr
                                              (invocation
                                                (OPAREN #f)
                                                (exprList
                                                  (expr (atom (NAME +)))
                                                  (optExprList
                                                    (exprList
                                                      (expr (atom (NAME x)))
                                                      (optExprList
                                                        (exprList
                                                          (expr (atom (NAME n)))
                                                          (optExprList))))))
                                                (CPAREN #f)))))
                                     (optExprList))
                                   (CBRACE #f))
                                 (optMethodList)))))
                           (CBRACE #f)))
                       (optExprList)))))

(module+ test ;; massive integration test
  (check-equal? (parse "
(def factorial
  (fun (n)
    (if (< n 0.9)
        1  ;; base case
        (factorial (- n 1) ;* recursive case *; ))))
(print (+ \"5! is \" (factorial 5)))")

                '(program
                  (exprList
                   (expr
                    (invocation
                     (OPAREN #f)
                     (exprList
                      (expr
                       (atom
                        (NAME def)))
                      (optExprList
                       (exprList
                        (expr
                         (atom
                          (NAME factorial)))
                        (optExprList
                         (exprList
                          (expr
                           (invocation
                            (OPAREN #f)
                            (exprList
                             (expr
                              (atom
                               (NAME fun)))
                             (optExprList
                              (exprList
                               (expr
                                (invocation
                                 (OPAREN #f)
                                 (exprList
                                  (expr
                                   (atom
                                    (NAME n)))
                                  (optExprList))
                                 (CPAREN #f)))
                               (optExprList
                                (exprList
                                 (expr
                                  (invocation
                                   (OPAREN #f)
                                   (exprList
                                    (expr ; if
                                     (atom
                                      (NAME if)))
                                    (optExprList
                                     (exprList
                                      (expr ; (< n 0.9)
                                       (invocation
                                        (OPAREN #f)
                                        (exprList
                                         (expr (atom (NAME <)))
                                         (optExprList
                                          (exprList
                                           (expr (atom (NAME n)))
                                           (optExprList
                                            (exprList
                                             (expr (atom (number (FLOAT 0.9))))
                                             (optExprList))))))
                                        (CPAREN #f)))
                                      (optExprList
                                       (exprList
                                        (expr (atom (number (INT 1))))
                                        (optExprList
                                         (exprList
                                          (expr
                                           (invocation
                                            (OPAREN #f)
                                            (exprList
                                             (expr (atom (NAME factorial)))
                                             (optExprList
                                              (exprList
                                               (expr
                                                (invocation
                                                 (OPAREN #f)
                                                 (exprList
                                                  (expr (atom (NAME -)))
                                                  (optExprList
                                                   (exprList
                                                    (expr (atom (NAME n)))
                                                    (optExprList
                                                     (exprList (expr (atom (number (INT 1)))) (optExprList))))))
                                                 (CPAREN #f)))
                                               (optExprList))))
                                            (CPAREN #f)))
                                          (optExprList))))))))
                                   (CPAREN #f)))
                                 (optExprList))))))
                            (CPAREN #f)))
                          (optExprList))))))
                     (CPAREN #f)))
                   (optExprList
                    (exprList
                     (expr
                      (invocation
                       (OPAREN #f)
                       (exprList
                        (expr
                         (atom (NAME print)))
                        (optExprList
                         (exprList
                          (expr
                           (invocation
                            (OPAREN #f)
                            (exprList
                             (expr
                              (atom
                               (NAME +)))
                             (optExprList
                              (exprList
                               (expr
                                (atom
                                 (STRING "5! is ")))
                               (optExprList
                                (exprList
                                 (expr
                                  (invocation
                                   (OPAREN #f)
                                   (exprList
                                    (expr
                                     (atom
                                      (NAME factorial)))
                                    (optExprList
                                     (exprList
                                      (expr
                                       (atom
                                        (number
                                         (INT 5))))
                                      (optExprList))))
                                   (CPAREN #f)))
                                 (optExprList))))))
                            (CPAREN #f)))
                          (optExprList))))
                       (CPAREN #f)))
                     (optExprList)))))))

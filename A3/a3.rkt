#lang racket

(require (only-in (file "lex.rkt") lex))

(module+ test
  (require (only-in rackunit
                    check-equal?))
  (check-equal? (parse "(define factorial
  (fun (n)
    (if (< n 0.9)
        1  ;; base case
        (factorial (- n 1) ;* recursive case *; ))))

(print (+ \"5! is \" (factorial 5)))") '(program
  (exprList
   (expr
    (invocation
     (OPAREN #f)
     (exprList
      (expr
       (atom
        (NAME define)))
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
                                     (exprList
                                      (expr (atom (number (INT 1))))
                                      (optExprList))))))
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

(lex "(define factorial
  (fun (n)
    (if (< n 0.9)
        1  ;; base case
        (factorial (- n 1) ;* recursive case *; ))))

(print (+ \"5! is \" (factorial 5)))")

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(define tokens (make-parameter '()))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(define (parse-program)
  (list 'program
        (parse-exprList)))

(define (parse-exprList)
  (list 'exprList
        (parse-expr)
        (parse-optExprList)))

(define (parse-expr)
  (if (atom-pending?)
      (list 'expr
            (parse-atom))
      (list 'expr
            (parse-invocation))))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

(define (parse-optExprList)
  (if (exprList-pending?)
      (list 'optExprList
            (parse-exprList))
      (list 'optExprList)))

(define (exprList-pending?)
  (or (atom-pending?)
      (check 'OPAREN)))

(define (parse-atom)
  (if (check 'NAME)
      (list 'atom
            (consume 'NAME))
      (if (check 'STRING)
          (list 'atom
                (consume 'STRING))
          (list 'atom
                (parse-number)))))

(define (parse-number)
  (if (check 'INT)
      (list 'number
            (consume 'INT))
      (list 'number
            (consume 'FLOAT))))

(define (parse-invocation)
  (list 'invocation
        (consume 'OPAREN)
        (parse-exprList)
        (consume 'CPAREN)))


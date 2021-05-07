#lang racket

(require (only-in (file "a3.rkt") parse))

(module+ test
  (require (only-in rackunit
                    check-equal?))
  (check-equal? (eval "5") 5)
  (check-equal? (eval "(* 7 8)") 56)
  (check-equal? (eval "(* 7 (- 8))") -56)
  (check-equal? (eval "(* (+ 1 2) (/ 8 4))") 6)
  (check-equal? (eval "(+ 1 2)") 3)
  (check-equal? (eval "(string=? \"abc\" (string-append \"a\" \"b\" \"c\"))") #t)
  (check-equal? (eval "(string<? \"abc\" (string-append \"a\" \"b\" \"b\"))") #f)
  (check-equal? (eval "(not (string<? \"abc\" (string-append \"a\" \"b\" \"b\")))") #t))

(define (eval input)
  (let ([tree (parse input)])
    (last (eval-program tree))))

(define (eval-program program-expr)
  ;; program := exprList)
  (let* ([child (second program-expr)])
        (eval-exprList child)))

(define (eval-exprList exprList-expr)
  ;; exprList := expr optExprList
  (let* ([label (first exprList-expr)]
        [expr (second exprList-expr)]
        [optExprList (third exprList-expr)])
    (cons (eval-expr expr) (eval-optExprList optExprList))))

(define (eval-optExprList optExprList-expr)
  ;; optExprList := ɛ | exprList
  (let* ([label (rest optExprList-expr)])
     (cond
      [(empty? label) '()]
      [else (eval-exprList (second optExprList-expr))])))

(define (eval-expr expr-expr)
  ;; expr := atom | invocation
  (let* ([child (second expr-expr)]
         [child-label (first child)])
    (cond
      [(equal? child-label       'atom) (eval-atom      child)]
      [(equal? child-label 'invocation) (eval-invocation child)])))

(define (eval-atom atom-expr)
  ;; atom := NAME | STRING | number
  (let* ([child (second atom-expr)]
         [child-label (first child)])
  (cond
      [(equal? child-label       'NAME)
        (cond
          [(equal? (second child) '+) +]
          [(equal? (second child) '-) -]
          [(equal? (second child) '*) *]
          [(equal? (second child) '/) /]
          [(equal? (second child) 'string-append) string-append]
          [(equal? (second child) 'string<?) string<?]
          [(equal? (second child) 'string=?) string=?]
          [(equal? (second child) 'not) not]
          [(equal? (second child) '=) =]
          [(equal? (second child) '<) <]
          [else (error "Bad name")])]
      [(equal? child-label     'STRING) (second child)]
      [else                             (eval-number child)])))

(define (eval-number number-expr)
  ;; number := INT | FLOAT
  (let* ([child (second number-expr)]
         [child-label (first child)])
    (if (equal? child-label 'INT) 
        (second child)
        (second child))))

(define (eval-invocation invocation-expr)
  ;; invocation := OPAREN exprList CPAREN
  (let* ([label (first invocation-expr)]
         [OPAREN (second invocation-expr)]
         [exprList (third invocation-expr)]
         [CPAREN (fourth invocation-expr)]
         [result (eval-exprList exprList)]
         [rater (first result)]
         [other (rest result)])
    (cond
      [(or (equal? rater +) (equal? rater -) (equal? rater *))
        (if (get-numbers other)
            (apply rater other)
            (error "Not all numbers"))]
      [(equal? rater /)
       (if (get-numbers other)
           (if (get-division other)
            (apply rater other)
            (error "Division by 0"))
           (error "Not all numbers"))]
      [(or (equal? rater string-append) (equal? rater string<?) (equal? rater string=?))
        (if (get-strings other)
            (apply rater other)
            (error "Not all strings"))]
      [else (apply rater other)])))

(define get-numbers
    (lambda (x)
     (if (null? x)
        #t
        (if (number? (car x))
            (get-numbers (cdr x))
            #f))))

(define get-division
  (lambda (x)
    (if (null? x)
        #t
        (if (< 0 (car x))
            (get-division (cdr x))
            #f))))

(define get-strings
    (lambda (x)
     (if (null? x)
        #t
        (if (string? (car x))
            (get-strings (cdr x))
            #f))))

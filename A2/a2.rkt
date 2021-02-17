#lang racket
;punctuation
;number literals
;string literals
;keywords
;names
;(regexp-match test "a")

(define (skip-match str) #f)

(define (token type [data #f])
  (list type data))

(define (punctuation-token str)
  (token
    (case str
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE]
      [(",") 'COMMA]
      [(";") 'SEMICOLON]
      [(".") 'PERIOD])))

(define (number-token str)
  (token 'NUM (string->number str)))

(define (name-or-keyword-token str)
  (case str
    [("read" "write" "def" "fun" "if" "not" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))


(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^//[^\n]+(\n|$)" skip-match) ; // comments
    (list #rx"^/\\*.*?\\*/" skip-match) ; // comments
    (list #rx"^[(){},;.]" punctuation-token)
    (list #rx"^[0-9]+(?=[\r\n\t (){},;.]|$)" number-token)
    (list #rx"^[A-Za-z]+|=|<|>|-(?=[\r\n\t (){},;.]|$)" name-or-keyword-token)))

(define (find-token str)
  (map (lambda (entry) (regexp-match (first entry) str)) re-table))

(define (find-substr lst)
  (if (not (equal? (first lst) #f))
      (first (first lst))
      (find-substr (rest lst))))

(define (to-token lst)
  (letrec ([helper
            (lambda (acc l)
              (if (empty? l)
                  '()
                  (if (equal? (first l) #f)
                      (helper (add1 acc) (rest l) )
                      (case acc
                        [(0) (skip-match (first (first l)))]
                        [(1) (skip-match (first (first l)))]
                        [(2) (skip-match (first (first l)))]
                        [(3) (punctuation-token (first (first l)))]
                        [(4) (number-token (first (first l)))]
                        [(5) (name-or-keyword-token (first (first l)))]))))])
    (helper 0 lst)))

(define (parse-substr s)
  (if (empty? (to-token (find-token s)))
      (list 'INVALID s)
      (to-token (find-token s))))

(define (reduce-string input str)
  (substring str (string-length input)))

(define (new-string s)
  (if (not (parse-substr s))
      (reduce-string (find-substr (find-token s)) s)
      (if (equal? (first (parse-substr s)) 'INVALID)
          ""
          (reduce-string (find-substr (find-token s)) s))))

(define (reduce-list lst)
  (if (empty? lst)
      '()
      (if (equal? (first lst) #f)
          (reduce-list (rest lst))
          (cons (first lst) (reduce-list (rest lst))))))

(define (create-list s)
  (if (equal? s "")
      '()
      (cons (parse-substr s) (lex (new-string s)))))

(define (lex s)
  (reduce-list (create-list s)))

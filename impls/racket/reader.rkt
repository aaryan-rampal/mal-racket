#lang racket

(require test-engine/racket-tests)

(provide read_str)

(define Reader%
  (class object%
    (super-new)
    (init-field tokens)
    (define/public (peek los)
      (first tokens))
    (define/public (next los)
      (begin (new this% (rest tokens))
             (first tokens)))))

(define (next)
  (0))

(define (peek los)
  (first los))

(define (read_str str)
  (local [(define reader (dynamic-instantiate Reader% (tokenize str)))]
    (read_form reader)))

;; use regex match, filter all the strings that satisfy NOT (EMPTY AND first
;; char = ';' and trim all items in list
(define (tokenize inp)
  (map string-trim
       (filter
        (λ (i)
          (not (and
                (string=? i "")
                (or (string=? i "")
                    (string=? (string-ref i 0 ) ";")))))
        (regexp-match*
         #px"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\\s\\[\\]
{}('\"`,;)]*)" inp))))







(check-expect (read_form (list "(" "+" "2" "3" ")"))
              (list "+" "2" "3"))
(check-expect (read_form (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")"))
              (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect (read_form (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")"))
              (list "+" "2" "3" (list "*" "3" "4")))

(define (read_form los)
  (local [(define (read_form0 lis rsf)
            (local [(define first
                      (if (not (empty? los))
                          (peek los)
                          ""))]
              (cond [(empty? lis) rsf]
                    [(string=? "(" first)
                     (if (empty? rsf)
                         (read_list (rest lis))
                         (cons rsf (read_list (rest lis))))]
                    [else
                     (read_form0 (rest lis)
                                 (append rsf (list (read_atom lis))))])))]
    (read_form0 los '())))



(check-expect (return_until_bracket_end (list "+" "2" "3" ")"))
              '())
(define (return_until_bracket_end los)
  (cond [(empty? los) 0]
        [(string=? ")" (peek los)) (rest los)]
        [else (return_until_bracket_end (rest los))]))






(check-expect (read_list (list "+" "2" "3" ")"))
              (list "+" "2" "3"))

(check-expect (read_form (list "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")"))
              (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect (read_list (list "2" "3" "(" "*" "3" "4" ")"))
              (list "2" "3" (list "*" "3" "4")))
(define (read_list los)
  (local [(define (read_list0 lis rsf)
            (local [(define first
                      (if (not (empty? lis))
                          (peek lis)
                          ""))]
              (cond [(empty? lis) (raise 'failed #t)]
                    [(string=? ")" first) rsf]
                    [else
                     (cons (read_form lis) (read_list0 (rest lis) (append rsf (list first))))])))]
    (read_list0 los '())))

(define (read_atom los)
  (first los))

;(tokenize "(+ 2 (* 3 4))")
;(read_list (list "(" "!" ")"))
;(read_form (tokenize  "(+ 2 (* 3 4))"))
;(read_str "(+ 2 4)")
(test)


#lang racket

(provide read_str)

(define (next)
  (0))

(define (peek los)
  (first los))

(define (read_str str)
  (read_form (tokenize str)))

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

(define (read_form los)
  (local [(define (fn-for-lis lis rsf)
            (local [(define first (peek lis))]
              (cond [(empty? lis) rsf]
                    [(string=? "(" first) (append rsf (read_list los))]
                    [else
                     (append rsf (list (read_atom los)))])))]
    (fn-for-lis los '())))

(define (read_list lis)
  (local [(define (fn-for-lis lis rsf)
            (local [(define first (peek lis))]
              (cond [(string=? ")" first) rsf]
                    [else
                     (read_form (rest lis))])))]
    (fn-for-lis lis '())))

(define (read_atom los)
  los)

(tokenize "(+ 2 (* 3 4))")
(read_form (tokenize  "(+ 2 (* 3 4))"))
(read_str "(+ 2 (* 3 4))")

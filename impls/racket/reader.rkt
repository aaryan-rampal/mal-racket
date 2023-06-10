#lang racket

(require test-engine/racket-tests)

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







(check-expect (read_form (list "(" "+" "2" "3" ")"))
              (list "+" "2" "3"))
(check-expect (read_form (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")"))
              (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect (read_form (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")"))
              (list "+" "2" "3" (list "*" "3" "4")))

(define (read_form los)
  (local [(define first
            (if (not (empty? los))
                (peek los)
                ""))
          
          (define (read_form0 lis rsf)
            (cond [(empty? lis) rsf]
                  [(string=? "(" first)
                   (read_form0 (rest lis)
                               (if (empty? rsf)
                                   (read_list (rest lis))
                                   (cons rsf (read_list (rest lis)))))]
                  [else
                   (read_form0 (rest lis)
                               (append rsf (list (read_atom lis))))]))
          
          (define (read_list lis rsf)
            (cond [(empty? lis) (raise 'failed #t)]
                  [(string=? ")" first) rsf]
                  [else
                   (read_list (rest lis)
                              (if (empty? rsf)
                                  (read_form0 (rest lis))
                                  (append rsf (read_form0 (rest lis)))))]))]
    (read_form0 los '())))



(check-expect (return_until_bracket_end (list "+" "2" "3" ")"))
              '())
(define (return_until_bracket_end los)
  (cond [(empty? los) 0]
        [(string=? ")" (peek los)) (rest los)]
        [else (return_until_bracket_end (rest los))]))






(check-expect (read_list (list "+" "2" "3" ")"))
              (list "+" "2" "3"))
(check-expect (read_list (list "2" "3" "(" "*" "3" "4" ")"))
              (list "2" "3" (list "*" "3" "4")))
(define (read_list los)
  (local [(define (fn-for-lis lis rsf)
            (local [(define first
                      (if (not (empty? lis))
                          (peek lis)
                          ""))]
              (cond [(empty? lis) (raise 'failed #t)]
                    [(string=? ")" first) rsf]
                    [else
                     (fn-for-lis (rest lis)
                                 (if (empty? rsf)
                                     (read_form (rest lis))
                                     (append rsf (read_form (rest lis)))))])))]
    (fn-for-lis los '())))

(define (read_atom los)
  (first los))

;(tokenize "(+ 2 (* 3 4))")
;(read_list (list "(" "!" ")"))
;(read_form (tokenize  "(+ 2 (* 3 4))"))
;(read_str "(+ 2 4)")
(test)


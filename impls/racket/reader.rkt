#lang racket

(require test-engine/racket-tests)

(provide read_str)

(define Reader%
  (class object%
    (super-new)
    (init-field tokens)
    (init-field i)
    (define/public (peek)
      (cond [(i >= (length tokens))] (raise 'failed #t)
            [else (list-ref tokens i)]))
    (define/public (next los)
      (begin (set! i (+ i 1))
             (peek)))))



(define (read_str str)
  (local [(define reader (new Reader% [tokens (tokenize str)] [i 0]))]
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







(check-expect (read_form (new Reader% [tokens (list "(" "+" "2" "3" ")")]
                              [i 0]))
              (list "+" "2" "3"))
(check-expect (read_form  (new Reader% [tokens (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")")]
                               [i 0]))
              (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect (read_form (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
                              [i 0]))
              (list "+" "2" "3" (list "*" "3" "4")))

(define (read_form reader)
  (local [(define (read_form0 rsf)
            (local [(define first (send reader peek))]
              (cond ;[(empty? lis) rsf]
                    [(string=? "(" first)
                     (if (empty? rsf)
                         (read_list reader)
                         (cons rsf (read_list reader)))]
                    [else
                     (read_form0 reader
                                 (append rsf (list (read_atom reader))))])))]
    (read_form0 '())))






(check-expect (read_list (new Reader% [tokens (list "(" "+" "2" "3" ")")]
                              [i 0]))
              (list "+" "2" "3"))
(check-expect (read_list  (new Reader% [tokens (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")")]
                               [i 0]))
              (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect (read_list (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
                              [i 0]))
              (list "+" "2" "3" (list "*" "3" "4")))

(define (read_list reader)
  (local [(define (read_list0 rsf)
            (local [(define first (send reader peek))]
              (cond ;[(empty? lis) (raise 'failed #t)]
                    [(string=? "(" first) (read_list0 (send reader next))]
                    [(string=? ")" first) rsf]
                    [else
                     (cons (read_form reader) (read_list0 (append rsf (list first))))])))]
    (read_list0 '())))

(define (read_atom reader)
  (send reader next))

;(tokenize "(+ 2 (* 3 4))")
;(read_list (list "(" "!" ")"))
;(read_form (tokenize  "(+ 2 (* 3 4))"))
;(read_str "(+ 2 4)")
(test)


#lang racket

(require test-engine/racket-tests)

(provide read_str)

(define Reader%
  (class object%
    (super-new)
    (init-field tokens)
    (init-field i)
    (define/public (peek)
      (cond [(end?) null]
            [else (list-ref tokens i)]))
    (define/public (peek_prev)
      (list-ref tokens (- i 1)))
    (define/public (next)
      (begin (set! i (+ i 1))
             (peek_prev)))
    (define/public (end?)
      (>= i (length tokens)))))



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





;(check-expect (read_form (new Reader% [tokens (list "+" "2" "3" ")")]
;                              [i 0]))
;              (list "+" "2" "3"))
(check-expect (read_form (new Reader% [tokens (list "(" "+" "2" "3" ")")]
                              [i 0]))
              (list "+" "2" "3"))
(check-expect
 (read_form
  (new Reader%
       [tokens (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")")]
       [i 0]))
 (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect
 (read_form
  (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
       [i 0]))
 (list "+" "2" "3" (list "*" "3" "4")))

(define (read_form reader)
  ;; rsf is a list of the arguments seen so far by read_form 
  (local [(define (read_form0 rsf reader)
            (local [(define first (send reader peek))]
              (cond [(send reader end?) rsf]
                    [(string=? "(" first)
                     (read_form0
                      (if (empty? rsf)
                          (read_list reader "(")
                          (cons rsf (cons (read_list reader "(") '())))
                      reader)]
                    [else
                     (read_form0
                      (append rsf (list (read_atom reader))) reader)])))]
    (read_form0 '() reader)))






(check-expect (read_list (new Reader% [tokens (list "(" "+" "2" "3" ")")]
                              [i 0]) "(")
              (list "+" "2" "3"))
(check-expect
 (read_list
  (new Reader%
       [tokens (list "(" "(" "+" "2" "3" ")" "(" "*" "3" "4" ")" ")")]
       [i 0]) "(")
 (list (list "+" "2" "3") (list "*" "3" "4")))
(check-expect
 (read_list (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
                 [i 0]) "(")
 (list "+" "2" "3" (list "*" "3" "4")))

(define (read_list reader frst)
  ;; rsf is a list of the arguments seen so far by read_list 
  (local [(define (read_list0 rsf)
            (local [(define first (if (send reader end?) ""
                                      (send reader next)))]
              (cond [(send reader end?)
                     (raise "Unexpected EOF: no \")\" to end \"(\".")]
                    [(not (string=? "(" first)) (raise "error")]
                    [(string=? "(" first) (read_seq reader "(" ")")]
                    [(string=? ")" first) (raise "Unexpected \")\"")])))]
    (read_list0 '())))



(check-expect (read_seq (new Reader% [tokens (list "(" "+" "2" "3" ")")]
                             [i 0]) "(" ")")
              (list "+" "2" "3"))
(check-expect (read_seq (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
                             [i 0]) "(" ")")
              (list "+" "2" "3" (list "*" "3" "4")))
(define (read_seq reader frst end)
  (local [(define (read_seq0 rsf)
            (local [(define token (send reader next))]
              (cond [(string=? token end) rsf]
                    [else
                     (read_seq0 (append rsf (list token)))])))]
    (read_seq0 '())))



(check-expect
 (read_atom
  (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
       [i 0])) "(")

(define (read_atom reader)
  (send reader next))

;(tokenize "(+ 2 (* 3 4))")
;(read_list (list "(" "!" ")"))
;(read_form (tokenize  "(+ 2 (* 3 4))"))
(read_form (new Reader% [tokens (list "+" "2" "3" ")")]
                [i 0]))
(test)


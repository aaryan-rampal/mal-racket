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
      (cond [(end?) null]
            [else (list-ref tokens (- i 1))]))
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
  (local [(define first (send reader peek))]
    (cond [(send reader end?) '()]
          [(string=? "(" first)
           (read_list reader "(")]
          [else
           (read_atom reader)])))






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
  (local [(define first (send reader next))]
    (cond [(send reader end?)
           (raise "Unexpected EOF: no \")\" to end \"(\".")]
          [(not (string=? "(" first))
           (raise "error")]
          [(string=? "(" first)
           (let ([lst (read_seq reader "(" ")")])
             (send reader next)
             lst)]
          [(string=? ")" first)
           (raise "Unexpected \")\"")])))



(check-expect (read_seq (new Reader% [tokens (list "+" "2" "3" ")")]
                             [i 0]) "(" ")")
              (list "+" "2" "3"))
(check-expect (read_seq (new Reader% [tokens (list "*" "3" "4" ")")]
                             [i 0]) "(" ")")
              (list "*" "3" "4"))
(check-expect (read_seq
               (new Reader%
                    [tokens (list "+" "2" "3" "(" "*" "3" "4" ")" ")")]
                    [i 0]) "(" ")")
              (list "+" "2" "3" (list "*" "3" "4")))

(define (read_seq reader frst end)
  (local [(define token (send reader peek))]
    (cond [(send reader end?)
           '()]
          [(string=? token end) '()]
          [else (cons (read_form reader) (read_seq reader "(" ")"))])))



(check-expect
 (read_atom
  (new Reader% [tokens (list "(" "+" "2" "3" "(" "*" "3" "4" ")" ")")]
       [i 0])) "(")

(define (read_atom reader)
  (if (send reader end?)
      (raise "error in read_atom")
      (send reader next)))

(test)


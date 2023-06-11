#lang racket

(provide pr_str)
(require test-engine/racket-tests)

(check-expect (pr_str (list '+ 1 2)) "(+ 1 2)")
(define (pr_str data)
  (cond [(number? data) (number->string data)]
        [(symbol? data) (symbol->string data)]
        [(list? data) (iterate data "")]
        [else data]))

(define (iterate los rsf)
  ;; rsf is accumulator: it's the value of the string so far
  (cond [(empty? los) (string-append "(" rsf ")")]
        [else
         (iterate (rest los) (string-append rsf (if (string=? "" rsf) "" " ") (pr_str (first los))))]))

(test)
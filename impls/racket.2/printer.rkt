#lang racket

(provide pr_str)

(define (pr_str data)
  (cond [(number? data) data]
        [(symbol? data) data]
        [(list? data) (iterate data "")]))

(define (iterate los rsf)
  ;; rsf is accumulator: it's the value of the string so far
  (cond [(empty? los) (string-append "(" rsf ")")]
        [else
         (iterate (rest los) (string-append rsf "" (first los)))]))

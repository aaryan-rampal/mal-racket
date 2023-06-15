#lang racket

(require readline)
(require readline/readline)
(require "reader.rkt" "printer.rkt")

;; READ
(define (READ str)
  (read_str str))

;; EVAL
(define (EVAL ast env)
  ast)

;; PRINT
(define (PRINT exp)
  (pr_str exp))

;; rep
(define (rep str)
  (PRINT (EVAL (READ str) "")))

(define (loop)
  (local [(define inp (readline "users> "))]
    (cond [(not (eof-object? inp))
           (with-handlers ([exn:fail? (lambda (exn)
                                      (displayln (exn-message exn))
                                      #f)])
             (displayln (rep inp)))
           (loop)])))

(loop)

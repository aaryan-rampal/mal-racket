#lang racket

(require "readline.rkt")

;; READ
(define (READ str)
  str)

;; EVAL
(define (EVAL ast env)
  ast)

;; PRINT
(define (PRINT exp)
  exp)

;; rep
(define (rep str)
  (PRINT (EVAL (READ str) "")))

(define (loop)
  (local [(define inp (readline "users> "))]
    (cond [(not (eof-object? inp))
           (display (string-append (rep inp) "\n")) (loop)])))

(loop)

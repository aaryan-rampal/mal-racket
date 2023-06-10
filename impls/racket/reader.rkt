#lang racket

(define (next)
  (0))

(define (peek)
  (0))

(define (read_str)
  (0))

(define (tokenize inp)
  (string-split inp #rx"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)"))

(define (read_form)
  (0))

(define (read_list)
  (0))

(define (read_atom)
  (0))
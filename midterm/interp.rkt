#lang racket
;; amount/interp.rkt

(provide interp)
(require "ast.rkt")
 
;; Expr -> Integer
;; Interpret given expression
(define (interp e)
  (match e
    [(Int i) i]))

(interp (Int 42))
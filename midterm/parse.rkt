#lang racket
;; amount/parse.rkt

(provide parse)
(require "ast.rkt")
 
;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [_            (error "Parse error")]))
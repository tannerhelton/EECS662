#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Val s)]
    [(? boolean?) (Val s)]
    [(list (? unop? u) e) (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [`(if, e1, e2, e3) (If (parse e1) (parse e2) (parse e3))]
    [`(cond ,@clauses (else, e)) (Cond (parse-cond clauses) (parse e))]
    [_ (error "Parse error!")]))

(define (parse-cond clauses)
  (map (lambda (clause)
               (match clause
                 [`[else, e] (list 'else (parse e))] 
                 [`[,p ,a] (list (parse p) (parse a))]))
             clauses))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero? - not)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and or %)))


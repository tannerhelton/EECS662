#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Val s)]
    [(? boolean?) (Val s)]
    [(list 'or e1 e2) (Or (parse e1) (parse e2))]
    [(list '- e) (UnOp 'neg (parse e))]
    [(list 'not e) (Not (parse e))]
    [(list '% e1 e2) (Mod (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3) (If (parse e1) (parse e2) (parse e3))]
    [`(cond ,@clauses) (parse-cond clauses)]
    [_ (error "Parse error!")]))

(define (parse-cond clauses)
  (Cond (map (lambda (clause)
               (match clause
                 [`[else ,e] (list 'else (parse e))] 
                 [`[,p ,a] (list (parse p) (parse a))]))
             clauses)))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero? neg)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and %)))


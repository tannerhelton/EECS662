#lang racket

(provide cond->if currify)

(require "ast.rkt" "parser.rkt")

(define (cond->if e)
  (match e
    [(Cond cs else-expr)
     (define (cond->if-helper clauses)
       (match clauses
         [(list) (transform else-expr)]
         [(cons (list pred action) rest)
          (If (transform pred) (transform action) (cond->if-helper rest))]))
     (cond->if-helper cs)]
    [_ (transform e)]))

(define (currify e)
  (match e
    [(Lam xs body) 
     (define (nest-lambdas variables)
       (match variables
         [(list) body] 
         [(list x) (Lam (list x) body)]
         [(cons x rest) (Lam (list x) (nest-lambdas rest))]))
     (nest-lambdas xs)]
    [(App ef es) 
     (define (nest-applications f args)
       (match args
         [(list) f] 
         [(list arg) (App f arg)]
         [(cons arg1 rest) (nest-applications (App f arg1) rest)]))
     (nest-applications (transform ef) (map transform es))]
    [_ (transform e)]))


(define (transform e)
  (match e
    [(Cond cs else-expr) (cond->if e)]
    [(If e1 e2 e3) (If (transform e1) (transform e2) (transform e3))]
    [(App f args) (App (transform f) (map transform args))]
    [(Lam xs body) (Lam xs (transform body))]
    [(BinOp op e1 e2) (BinOp op (transform e1) (transform e2))]
    [(UnOp op e1) (UnOp op (transform e1))]
    [(Let x e1 e2) (Let x (transform e1) (transform e2))]
    [_ e]))


(module+ test
  (require rackunit)

  (check-equal? (unparse (cond->if (parse '(cond [(zero? (- 6 5)) 1]
                                                 [(<= 6 7)        2]
                                                 [else            3]))))
                '(if (zero? (- 6 5)) 1 (if (<= 6 7) 2 3)))

  (check-equal? (unparse (currify (parse '((λ (x y) (+ x y)) 2 3))))
                '(((λ (x) (λ (y) (+ x y))) 2) 3)))

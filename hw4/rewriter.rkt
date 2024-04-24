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
    [(App f args) (curry-args (curry-application f args) args)] 
    [(Lam params es) (curry-application e '())]
    [(BinOp op e1 e2) (BinOp op (currify e1) (currify e2))] 
    [(UnOp op e2) (UnOp op (currify e2))] 
    [(If e1 e2 e3) (If (currify e1) (currify e2) (currify e3))] 
    [(Cond cs x) (Cond (map (λ (c) (list (currify (first c)) (currify (second c)))) cs) (currify x))] 
    [(Let x e2 e3) (Let x (currify e2) (currify e3))]
    [(Var x) e] 
    [(list x) (map currify (list x))]
    [e e]))

(define (transform e)
  (display "Transforming expression: ") (displayln e)
  (match e
    [(Cond cs else-expr) (cond->if e)]
    [(If e1 e2 e3) (If (transform e1) (transform e2) (transform e3))]
    [(App f args) (App (transform f) (map transform args))] 
    [(Lam xs body) (currify (Lam xs body))]  
    [(BinOp op e1 e2) (BinOp op (transform e1) (transform e2))]
    [(UnOp op e1) (UnOp op (transform e1))]
    [(Let x e1 e2) (Let x (transform e1) (transform e2))]
    [_ e]))

(define (curry-application f args)
  (match f
    [(Lam (list param rest) body) (Lam (list param) (curry-application (Lam (list rest) body) args))] 
    [(Lam param body) f] 
    [(App e as) (curry-args (curry-application e as) as)] 
    [f f])) 

(define (curry-args curried args)
(match args
    [(list a rest) (curry-args (App curried (list a)) (currify rest))] 
    ['() (App curried '())] 
    [(list a) (App curried (list a))] 
    [a (App curried (list a))]))

(module+ test
  (require rackunit)

  ;; Basic zero-parameter lambda
  (check-equal? (unparse (currify (parse '(λ () 5))))
                '(λ () 5))

  ;; Currying and application in a let expression
  (check-equal? (unparse (currify (parse '(let ((div (λ (x y) (/ x y)))) (div 4 2)))))
                '(let ((div (λ (x) (λ (y) (/ x y))))) ((div 4) 2)))

  ;; Currying and application with an if condition
  (check-equal? (unparse (currify (parse '(if #t 4 ((λ (x y) (/ x y)) 4 2)))))
                '(if #t 4 (((λ (x) (λ (y) (/ x y))) 4) 2)))

  ;; Complex nested applications with multiple levels of lambdas
  (check-equal? (unparse (currify (parse '(((λ (x y) (λ (z) (+ z (/ x y)))) 4 2) 5))))
                '((((λ (x) (λ (y) (λ (z) (+ z (/ x y))))) 4) 2) 5))

  ;; Nested currying with further function applications
  (check-equal? (unparse (currify (parse '((λ (x y) (/ x y)) 4 ((λ (x y) (+ x y)) 2 3)))))
                '(((λ (x) (λ (y) (/ x y))) 4) (((λ (x) (λ (y) (+ x y))) 2) 3))))

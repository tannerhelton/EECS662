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
  (define (curry-lambda xs body)
    (if (null? xs)
        (begin
          (displayln "Currying terminal lambda body")
          (transform body))
        (let ([first (car xs)]
              [rest (cdr xs)])
          (displayln (format "Currying lambda with arg: ~a" first))
          (Lam (list first) (curry-lambda rest body)))))

  (define (curry-application f args)
    (define transformed-f (transform f))
    (define transformed-args (map transform args))
    (displayln "Currying application")
    (foldr (lambda (arg acc) (App acc arg)) transformed-f transformed-args))

  (match e
    [(Lam xs body)
     (displayln (format "Currying lambda with args: ~a" xs))
     (curry-lambda xs body)]
    [(App f args)
     (displayln (format "Currying application of: ~a with args: ~a" f args))
     (curry-application f args)]
    [_ (transform e)]))

(define (transform e)
  (match e
    [(Cond cs else-expr) (cond->if e)]
    [(If e1 e2 e3) (If (transform e1) (transform e2) (transform e3))]
    [(App f args) (App (transform f) (map transform args))] 
    [(Lam xs body) (currify (Lam xs body))]  
    [(BinOp op e1 e2) (BinOp op (transform e1) (transform e2))]
    [(UnOp op e1) (UnOp op (transform e1))]
    [(Let x e1 e2) (Let x (transform e1) (transform e2))]
    [_ e]))

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

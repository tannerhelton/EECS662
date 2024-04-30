#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp)

;; interp :: Expr -> Int
(define (interp e)
  (match e
    [(Val v) v]
    [(UnOp u e) (interp-unop u e)]
    [(BinOp b e1 e2) (interp-binop b e1 e2)]
    [(If e1 e2 e3) (interp-if e1 e2 e3)]
    [(Or e1 e2) (let ([v1 (interp e1)]) (if (equal? v1 #f) (interp e2) v1))]
    [(Not e) (not (interp e))]
    [(Mod e1 e2) (remainder (interp e1) (interp e2))]
    [(Cond clauses) (interp-cond clauses)]))

(define (interp-unop u e)
  (match u
    ['add1 (add1 (interp e))]
    ['sub1 (sub1 (interp e))]
    ['zero? (zero? (interp e))]
    ['neg (- (interp e))]))

(define (interp-binop b e1 e2)
  (match b
    ['+ (+ (interp e1) (interp e2))]
    ['- (- (interp e1) (interp e2))]
    ['* (* (interp e1) (interp e2))]
    ['/ (quotient (interp e1) (interp e2))]
    ['<= (<= (interp e1) (interp e2))]
    ['and (and (interp e1) (interp e2))]
    ['% (remainder (interp e1) (interp e2))]))

(define (interp-cond clauses)
  (for/or ([clause (in-list clauses)])
    (match clause
      [(list 'else e) (interp e)]
      [(list p a) (if (interp p) (interp a) #f)])))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(module+ test
  (check-eqv? (interp (parse '(or #f (+ 42 1)))) 43)
  (check-eqv? (interp (parse '(- 5))) -5)
  (check-eqv? (interp (parse '(not #t))) #f)
  (check-eqv? (interp (parse '(% 10 3))) 1)
  (check-eqv? (interp (parse '(cond [(zero? 0) 1] [else 2]))) 1))
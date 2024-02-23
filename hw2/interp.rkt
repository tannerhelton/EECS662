#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp)

(define (interp e)
  (match e
    [(Val v) v]
    [(UnOp u e) (interp-unop u e)]
    [(BinOp b e1 e2) (interp-binop b e1 e2)]
    [(If e1 e2 e3) (interp-if e1 e2 e3)]
    [(Cond clauses eClause) 
     (define (eval-cond-clauses clauses)
       (cond
         [(null? clauses) (interp eClause)]
         [else
          (let* ([clause (first clauses)]
                 [condition (first clause)]
                 [action (second clause)])
            (if (eq? (interp condition) #t)
                (interp action)
                (eval-cond-clauses (rest clauses))))]))
     (eval-cond-clauses clauses)])) 



(define (interp-unop u e)
  (match u
    ['add1 (add1 (interp e))]
    ['sub1 (sub1 (interp e))]
    ['zero? (match (interp e)
              [0 #t]
              [_ #f])]))

(define (interp-binop b e1 e2)
  (match b
    ['+ (+ (interp e1) (interp e2))]
    ['- (- (interp e1) (interp e2))]
    ['* (* (interp e1) (interp e2))]
    ['/ (quotient (interp e1) (interp e2))]
    ['<= (<= (interp e1) (interp e2))]
    ['and (match (interp e1)
            [#f #f]
            [? (interp e2)])]))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(module+ test
  (check-eqv? (interp (parse '(+ 42 (sub1 34)))) 75)
  (check-eqv? (interp (parse '(zero? (- 5 (sub1 6))))) #t)
  (check-eqv? (interp (parse '(if (zero? 0) (add1 5) (sub1 5)))) 6)
  (check-eqv? (interp (parse '(cond [(zero? 0) 5] [else 6]))) 5))

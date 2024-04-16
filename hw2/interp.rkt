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
    [(Cond cs e) (interp-pred cs e)]))

(define (interp-unop u e)
  (match u
    ['add1 (add1 (interp e))]
    ['sub1 (sub1 (interp e))]
    ['zero? (match (interp e)
              [0 #t]
              [_ #f])]
    ['- (- (interp e))]
    ['not (match (interp e)
            [#f #t]
            [_  #f])]))

(define (interp-binop b e1 e2)
  (match b
    ['+ (+ (interp e1) (interp e2))]
    ['- (- (interp e1) (interp e2))]
    ['* (* (interp e1) (interp e2))]
    ['/ (quotient (interp e1) (interp e2))]
    ['<= (<= (interp e1) (interp e2))]
    ['and (match (interp e1)
            [#f #f]
            [_ (interp e2)])]
    ['or (match (interp e1)
           [#f (interp e2)]
           [x  x])]
    ['% (remainder (interp e1) (interp e2))]))

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(define (interp-pred cs e)
  (match cs
    ['() (interp e)]
    [(cons `(,pred ,curr) rest) (match (interp pred)
                                  [#f (interp-pred rest e)]
                                  [_  (interp curr)])]))

(module+ test
  (check-eqv? (interp (parse '(+ 42 (sub1 34)))) 75)
  (check-eqv? (interp (parse '(zero? (- 5 (sub1 6))))) #t)
  (check-eqv? (interp (parse '(if (zero? 0) (add1 5) (sub1 5)))) 6)

  (check-eqv? (interp (parse '(or #f 2))) 2)
  (check-eqv? (interp (parse '(or #t 2))) #t)
  (check-eqv? (interp (parse '(or 4 2))) 4)
  (check-eqv? (interp (parse '(- 42))) -42)
  (check-eqv? (interp (parse '(- -42))) 42)
  (check-eqv? (interp (parse '(not -42))) #f)
  (check-eqv? (interp (parse '(not #t))) #f)
  (check-eqv? (interp (parse '(not #f))) #t)
  (check-eqv? (interp (parse '(% 5 2))) 1)
  (check-eqv? (interp (parse '(% -5 2))) -1)

  (check-eqv? (interp (parse '(cond [1  2] [4 5] [else 3]))) 2)
  (check-eqv? (interp (parse '(cond [#f 2] [4 5] [else 3]))) 5)
  (check-eqv? (interp (parse '(cond [else 3]))) 3)
  (check-eqv? (interp (parse '(cond [(zero? (- 6 5)) 1]
                                    [(<= 6 7)        2]
                                    [else            3]))) 2))

#lang racket

(struct Var (x) #:prefab)
(struct Lam (x e) #:prefab)
(struct App (f arg) #:prefab)
(require rackunit)

(define (parse s)
  (match s
    [(? symbol?) (Var s)]
    [`(λ (,x) ,e) (Lam x (parse e))]
    [`(lambda (,x) ,e) (Lam x (parse e))]
    [(list f arg) (App (parse f) (parse arg))]))

(define (unparse e)
  (match e
    [(Var x) x]
    [(Lam x e) (list 'λ (list x) (unparse e))]
    [(App f arg) (list (unparse f) (unparse arg))]))

(define (free? bound id e)
  (match e
    [(Var x) (and (not (member id bound)) (eq? x id))]
    [(Lam x e) (free? (cons x bound) id e)]
    [(App f arg) (or (free? bound id f) (free? bound id arg))]))


; z has to be fresh
(define (alpha-reduce M x z)
  (match M
    [(Var y) (if (eq? x y) (Var z) M)]
    [(Lam y e) (if (eq? y x) (Lam y e) (Lam y (alpha-reduce e x z)))]
    [(App f arg) (App (alpha-reduce f x z) (alpha-reduce arg x z))]))


(define (beta-reduce M x N)
  (match M
    [(Var y) (if (eq? x y) N M)]
    [(Lam y e) (if (eq? y x) (Lam y e) (Lam y (beta-reduce e x N)))]
    [(App f arg) (App (beta-reduce f x N) (beta-reduce arg x N))]))


(module+ test
  (require rackunit)

  (check-true  (free? '()  'x (parse '(x ((λ (x) x) y)))))
  (check-false (free? '(x) 'x (parse '(x ((λ (x) x) y)))))
  (check-false (free? '()  'x (parse '(z ((λ (x) x) y)))))

  (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'x 'z))
                '(λ (y) z))
  (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'y (gensym)))
                '(λ (y) x))

  (check-equal? (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y)))
              '(λ (y) y))
  
  (check-equal? (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y)))
              '(λ (y) y)))

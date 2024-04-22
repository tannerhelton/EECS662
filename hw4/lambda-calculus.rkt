#lang racket

(provide parse unparse free? alpha-reduce beta-reduce)

(struct Var (x) #:prefab)
(struct Lam (x e) #:prefab)
(struct App (f arg) #:prefab)

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
    [(Var x) (and (eq? x id) (not (member x bound)))]
    [(Lam x body)
     (if (eq? x id)
         (free? (cons x bound) id body)  
         (free? (cons x bound) id body))]
    [(App f arg)
     (or (free? bound id f) (free? bound id arg))]
    [_ #f]))

; z has to be fresh
(define (alpha-reduce M x z)
(match M
    [(Var v) (if (eq? v x) (Var z) M)]
    [(Lam v body)
     (if (eq? v x)
         (Lam v body)  
         (Lam v (alpha-reduce body x z)))]
    [(App f arg)
     (App (alpha-reduce f x z) (alpha-reduce arg x z))]
    [_ M]))


(define (free-variables exp bound)
  (match exp
    [(Var x) (if (member x bound) '() (list x))]
    [(Lam x body) (free-variables body (cons x bound))]
    [(App f arg) (union (free-variables f bound) (free-variables arg bound))]
    [_ '()]))

(define (union a b)
  (remove-duplicates (append a b)))

(define (beta-reduce M x N)
  (match M
    [(Var v) (if (eq? v x) N M)]
    [(Lam v body)
     (if (eq? v x)
         (Lam v body)  
         (let* ((free-in-N (free-variables N '()))  
                (fresh-v (if (member v free-in-N) (gensym) v)))
           (Lam fresh-v (beta-reduce (alpha-reduce body v fresh-v) x (alpha-reduce N x fresh-v)))))]
    [(App f arg)
     (App (beta-reduce f x N) (beta-reduce arg x N))]
    [_ M]))


(module+ test
  (require rackunit)

  (check-true  (free? '()  'x (parse '(x ((λ (x) x) y)))))
  (check-false (free? '(x) 'x (parse '(x ((λ (x) x) y)))))
  (check-false (free? '()  'x (parse '(z ((λ (x) x) y)))))

  (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'x 'z))
                '(λ (y) z))
  (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'y (gensym)))
                '(λ (y) x))

  (check-equal? (unparse (beta-reduce (parse '(x x)) 'x (parse '(λ (x) (x x)))))
                '((λ (x) (x x)) (λ (x) (x x))))
  (check-match (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y)))
                `(λ (,(? symbol?)) y)))

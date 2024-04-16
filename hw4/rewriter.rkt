#lang racket

; (require "ast.rkt" "parser.rkt")

(define (cond->if e)
  ; (match e
  ;   [(Cond cs else-part)
  ;    (define (cond-to-if cs)
  ;      (match cs
  ;        ['() else-part]
  ;        [(cons (list test expr) rest)
  ;         `(If ,(parse test) ,(parse expr) ,(cond-to-if rest))]))
  ;    (cond-to-if cs)]
  ;   [_ e])
  )

(define (currify e)
  ; (match e
  ;   [(Lam xs body)
  ;    (define (curry-lam xs body)
  ;      (match xs
  ;        [(list) body]
  ;        [(list x) `(Lam (,x) ,body)]
  ;        [(cons x xs) `(Lam (,x) ,(curry-lam xs body))]))
  ;    (curry-lam xs (parse body))]
  ;   [(App f args)
  ;    (define (curry-app args)
  ;      (match args
  ;        [(list) f]
  ;        [(list arg) `(App ,f ,arg)]
  ;        [(cons arg rest) `(App ,(curry-app rest) ,arg)]))
  ;    (curry-app args)]
  ;   [_ e])
  )


; (module+ test
;   (require rackunit)

;   (check-equal? (unparse (cond->if (parse '(cond [(zero? (- 6 5)) 1]
;                                                  [(<= 6 7)        2]
;                                                  [else            3]))))
;                 '(if (zero? (- 6 5)) 1 (if (<= 6 7) 2 3)))

;   (check-equal? (unparse (currify (parse '((λ (x y) (+ x y)) 2 3))))
;                 '(((λ (x) (λ (y) (+ x y))) 2) 3)))

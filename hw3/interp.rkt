#lang racket

(require "ast.rkt" "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp-prog e)))

;; interp :: Defn -> Env -> Expr -> Val
(define (interp D E e)
  (match e
    [(Val v)         v]
    [(Var x)         (lookup D E x)]
    [(UnOp u e)      (interp-unop D E u e)]
    [(BinOp b e1 e2) (interp-binop D E b e1 e2)]
    [(If e1 e2 e3)   (interp-if D E e1 e2 e3)]
    [(Let xs e)      (interp D (store-xs D E xs) e)]
    [(Let* xs e)     (interp D (store* D E xs) e)]
    [(Lam xs e)      (interp-lam D E xs e)]
    [(App e es)      (interp-app D E e es)]
    ))


;; interp-lam :: Defn -> Env -> Vars -> Expr -> Val
(define (interp-lam D E xs body)
  (λ (aargs) (if (eq? (length aargs) (length xs))
    (interp D (append (zip xs aargs) E) body) (raise (Err "arity mismatch")))))

;; interp-app :: Defn -> Env -> Expr -> Exprs -> Val
(define (interp-app D E f es)
    (let ([fn   (interp D E f)]
          [args (map (λ (arg)
                       (interp D E arg)) es)])
         (fn args)))

;; interp-prog :: Prog -> Val
(define (interp-prog prog)
  (match prog
    [(Prog D e) (interp D '() e)]))

;; interp-unop :: Defn -> Env -> UnOp -> Val
(define (interp-unop D E u e)
  (match u
    ['add1  (match (interp D E e)
              [(? integer? i) (add1 i)]
              [_              (raise (Err "add1 expects int"))])]
    ['sub1  (match (interp D E e)
              [(? integer? i) (sub1 i)]
              [_              (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp D E e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Defn -> Env -> BinOp -> Expr -> Expr -> Val
(define (interp-binop D E b e1 e2)
  (match b
    ['+ (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _)                             (raise (Err "+ requires int"))])]

    ['- (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _)                             (raise (Err "- requires int"))])]

    ['* (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _)                             (raise (Err "* requires int"))])]

    ['/ (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _)                             (raise (Err "/ requires int"))])]

    ['<= (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _)                             (raise (Err "<= requires int"))])]

    ['and (match (interp D E e1)
            [#f #f]
            [?  (interp D E e2)])]))

;; interp-if :: Defn -> Env -> Expr -> Expr -> Expr -> Val
(define (interp-if D E e1 e2 e3)
  (match (interp D E e1)
    [#f (interp D E e3)]
    [_  (interp D E e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define (store E x v)
  (cons (list x v) E))

(define (lookup D E x)
  (match E
    ['()                      (lookup-defn D D x)]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup D rest x))]))

(define (lookup-defn D defns x)
  (match defns
    ['()                          (raise (Err
                                          (string-append "Unbound identifier: "
                                                         (symbol->string x))))]
    [(cons (Defn f xs body) rest) (if (eq? f x)
                                      (λ (aargs) (if (eq? (length aargs) (length xs)) (interp D (zip xs aargs) body) (raise (Err (format "~a: arity mismatch" f)))))
                                      (lookup-defn D rest x))]
    [(cons (DefnV y e) rest)      (if (eq? x y)
                                      (interp D '() e)
                                      (lookup-defn D rest x))]))

(define (store-xs D E xs)
  (match xs
    ['() E]
    [xs (append (store-new D E '() xs) E)]))

(define (store-new D E-old E-new xs)
  (match xs
    ['() E-new]
    [(cons head tail) (cond
                [(defined? D E-new (var-str (car head))) 
                    (raise (Err (string-append "let: duplicate identifier in: " (symbol->string (var-str (car head))))))]
                [else (store-new D E-old (cons (list (var-str (car head)) (interp D E-old (cdr head))) E-new) tail)])]))

(define (store* D E xs)
  (match xs
    ['() E]
    [(cons head tail) 
      (store* D (cons (list (var-str (car head)) 
      (interp D E (cdr head))) E) tail)]))

(define (var-str v)
  (match v
    [`#s(Var ,x) x]
    [`(#s(Var ,x)) x]))

(define (defined? D E x)
  (match E
    ['() #f]
    [(cons (list y val) rest) (if (eq? x y) #t (defined? D rest x))]))

      
#lang racket

(provide Val UnOp BinOp If Err Err?
         Let Let* Var App Lam Defn DefnV Prog)

; type Values :=
;   | (Val v)
;   | (Lam xs e)
(struct Val   (v)        #:prefab)
(struct Lam   (xs e)     #:prefab)

; type Expr :=
;   | Values
;   | (Var   x)
;   | (UnOp  u e)
;   | (BinOp u e)
;   | (If    e e e)
;   | (Let   xs e)
;   | (Let*  xs e)
;   | (App   e e)
(struct Var   (x)        #:prefab)
(struct UnOp  (u e)      #:prefab)
(struct BinOp (b e1 e2)  #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Let   (xs e) #:prefab) 
(struct Let*  (xs e) #:prefab)
(struct App   (x args)   #:prefab)

; type Defn :=
;   | (Defn  x xs e)
;   | (DefnV x e)
(struct Defn  (x xs e)   #:prefab)
(struct DefnV (x e)      #:prefab)

; type Prog := (Prog Defns Expr)
(struct Prog  (defns e)  #:prefab)

(struct Err   (err)      #:prefab)

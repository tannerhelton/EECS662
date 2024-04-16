#lang racket

(provide Val UnOp BinOp If Cond)

(struct Val (v) #:prefab)
(struct UnOp (u e) #:prefab)
(struct BinOp (b e1 e2) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Cond (cs e) #:prefab)

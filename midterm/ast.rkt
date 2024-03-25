#lang racket
;; amount/ast.rkt

(provide Int)
 
;; type Expr = (Int Integer)
(struct Int (i) #:prefab)
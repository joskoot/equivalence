#lang racket

(provide eqv-classes equivalence?)

(define (eqv-classes s eq)
 (define make-immutable-dict
  (let-values (((a b c d e f g) (make-custom-hash-types eq))) e))
 (for/fold
  ((h (make-immutable-dict))
   #:result (dict-values h))
  ((element (in-set s)))
  (dict-set h element (set-add (dict-ref h element (set)) element))))

(define (equivalence? f s)
 (define set-s (in-set s))
 (and
  ;reflexivity
  (for/and ((x set-s)) (f x x))
  ;symmetry
  (for/and ((x set-s) (y set-s))
   (eq? (and (f x y) #t) (and (f y x) #t)))
  ;transitivity
  (for/and ((x set-s) (y set-s) (z set-s) #:when (and (f x y) (f y z)))
   (f x z))))

#lang racket

; Procedure: (eqv-classes (lst list?) (eq (-> any/c any/c))
;            -> (listof list?)
; Every sublist shows an equivalence-class
; of the elements of lst using equivalence relation eq.
; eq must be defined for all pairs of elements of the lst,
; but may be unpredictable for other arguments.
; eq is not checked to be an equivalence relation for lst.

(define (eqv-classes lst eq)
 (define make-immutable-dict
  (let-values (((a b c d e f g) (make-custom-hash-types eq))) e))
 (for/fold
  ((h (make-immutable-dict))
   #:result (dict-values h))
  ((element (in-list lst)))
  (dict-set h element (cons element (dict-ref h element '())))))

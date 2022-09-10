#lang racket
(require redex)
(define (count-zeros lst)
  ; scipping leading 0's
  (define (skip-leading lst)
    (cond
      [(empty? lst) empty]
      [else
       (cond
         [(= 0 (first lst)) (skip-leading (rest lst))]
         [else lst])]))
  ; helper for doing a tail recursion
  (define (helper lst current)
    (cond
      [(empty? lst) current]
      [else
       (cond
         [(= 0 (first lst)) (helper (rest lst) (+ 1 current))]
         [else (helper (rest lst) current)])]))
  (helper (skip-leading lst) 0))
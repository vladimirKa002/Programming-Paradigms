#lang racket
(require redex)
(define (binary-to-decimal lst)
  ; helper for doing a tail recursion
  (define (helper lst current)
    (cond
      [(empty? lst) current]
      [else (helper (rest lst)
                    (+ (* 2 current) (first lst)))]))
  (helper lst 0))
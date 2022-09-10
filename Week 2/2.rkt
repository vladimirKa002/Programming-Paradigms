#lang racket
(require redex)
(define (alternating-sum-expl lst)
  (define (multiplier-helper lst mult)
    (cond
      [(empty? lst) 0]
      [else (+ (* mult (first lst))
               (multiplier-helper (rest lst) (* mult -1)))]))
  (multiplier-helper lst 1))

(define (alternating-sum-tail lst)
  (define (multiplier-helper lst current mult)
    (define (helper lst current mult)
      (cond
        [(empty? lst) current]
        [else (helper (rest lst)
                      (+ current (first lst)))]))
    (cond
      [(empty? lst) current]
      [else (multiplier-helper (rest lst) (+ (* mult (first lst)) current) (* mult -1))]))
  (multiplier-helper lst 0 1))
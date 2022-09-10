#lang racket
(require redex)
(define (decrement lst)
  (define (skip-leading lst)
    (cond
      [(empty? lst) (list 0)]
      [else
       (cond
         [(= 0 (first lst)) (skip-leading (rest lst))]
         [else lst])]))
  (define (is-zero? lst)
    (cond
      [(empty? lst) #t]
      [else
       (cond
         [(= 0 (first lst)) (is-zero? (rest lst))]
         [else #f])]))
  (define (helper lst result sub)
    (cond
      [(empty? lst) result]
      [else
       (cond
         [(= sub 1)
          (cond
            [(= (first lst) 1) (helper (rest lst) (cons 0 result) 0)]
            [else (helper (rest lst) (cons 1 result) 1)])]
         [else (helper (rest lst) (cons (first lst) result) 0)])]))
  (cond
    [(is-zero? lst) '(0)]
    [else (skip-leading (helper (reverse lst) empty 1))]))
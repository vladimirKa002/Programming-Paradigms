#lang racket
(require redex)
(define (encode-with-lengths lst)
  (define (skip-leading lst)
    (cond
      [(empty? lst) empty]
      [else
       (cond
         [(= 0 (first lst)) (skip-leading (rest lst))]
         [else lst])]))
  (define (helper lst result cur-num cur-am)
    (cond
      [(empty? lst) (cons cur-am result)]
      [else
       (cond
         [(= cur-num (first lst)) (helper (rest lst) result cur-num (+ 1 cur-am))]
         [else (helper (rest lst) (cons cur-am result) (first lst) 1)])]))
  (rest (reverse (helper (skip-leading lst) empty 2 0))))
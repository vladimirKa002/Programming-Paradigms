#lang racket
(require redex)
(define (binary-odd? lst)
  (cond
      [(empty? lst) #f]
      [else
       (cond
         [(= 0 (last lst)) #f]
         [else #t])]))
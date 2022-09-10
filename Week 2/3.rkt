#lang racket
(require redex)
(define (dec n) (- n 1))
(define (f n)
  (cond
    [(<= n 2) (- 10 n)]
    [else (* (f (dec (dec n))) (f (dec n)))]))

; For case (f 3):
; (f 3) ->
; (* (f (dec (dec 3)))(f (dec 3))) ->
; (* (f (dec (- 3 1)))(f (dec 3))) ->
; (* (f (dec 2))(f (dec 3))) ->
; (* (f (- 2 1))(f (dec 3))) ->
; (* (f 1)(f (dec 3))) ->
; (* (- 10 1) (f (dec 3))) ->
; (* 9 (f (dec 3))) ->
; (* 9 (f (- 3 1))) ->
; (* 9 (f 2)) ->
; (* 9 (- 10 2)) ->
; (* 9 8) ->
; 72.
#lang racket
(require redex)

(define (replicate n seq)
  (cond
    [(= 0 n) '()]
    [else (cons seq (replicate (- n 1) seq))]))

; The result will not have a dot between values because
; Racket uses a different notation for displaying such values.
; Here we have a list of 1 lists (same as pair of lists)
; We could replace it with (cons l1 l2), then the result will look
; a bit different: ((1 2 3) 4 5 6), so it is just a matter of
; Racket notation.
(define (split lst n)
  (define (helper l1 l2 cur)
    (cond
      [(or (empty? l2) (= cur n)) (list l1 l2)]
      [else
       (helper (append l1 (list (first l2))) (rest l2) (+ 1 cur))]))
  (helper '() lst 0))

(define (chunks lst n)
  (let ((splitted (split lst n)))
    (cond
      ; Need to return last element bounded in list, because otherwise
      ; the result will look like:
      ; '((1 2 3) (4 5 6) 7 8)
      [(empty? (last splitted)) (list (first splitted))]
      [else
       (cons (first splitted) (chunks (last splitted) n))])))

(define (windows lst n)
  (define (helper res lst)
    (cond
      [(> n (length lst)) '()]
      [else
       (cons (first (split lst n)) (helper res (rest lst)))]))
  ; If length of the input list is less than n
  ; then return the list itself, otherwise create windows
  (cond
    [(>= n (length lst)) lst]
    [else (helper '() lst)]))

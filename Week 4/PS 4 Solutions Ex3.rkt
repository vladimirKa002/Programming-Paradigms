#lang racket
(require redex)
(define (max lst)
  (define (helper lst)
    (foldl (lambda (x val)
      (cond
        [(> x val) x]
        [else val])) -inf.0 lst))
  ; If list is empty, return empty list
  (cond
    [(= 0 (length lst)) '()]
    [else (helper lst)]))

(define (second-max lst)
  (define (helper lst)
    (max (remove (max lst) lst)))
  ; If list has less than 2 elements, return empty list
  (cond
    [(> 2 (length lst)) '()]
    [else (helper lst)]))

(define (top-3 lst)
  (foldl (lambda (x val)
    (cond
      [(< (- (length lst) 3)
        (foldl (lambda (y val)
          (cond
            [(<= y x) (+ val 1)]
            [else val])) 0 lst)) (append val (list x))]
      [else val])) '() lst))

; Additional function for top-n elements. Created for interest.
(define (top-n lst n)
  (foldl (lambda (x val)
    (cond
      [(< (- (length lst) n)
        (foldl (lambda (y val)
          (cond
            [(<= y x) (+ val 1)]
            [else val])) 0 lst)) (append val (list x))]
      [else val])) '() lst))

(define (group lst)
  (foldl (lambda (x res)
     (cond
      [(and (< 0 (length res)) (equal? x (first (last res))))
       (append (drop-right res 1) (list (cons x (last res))))]
      [else (append res (list (list x)))])) '() lst))

(define (cumulative-sums lst)
  (foldl (lambda (x res)
     (append res (list (+ x (last res))))) '(0) lst))
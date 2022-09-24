#lang racket
(require redex)
(define (binary-to-decimal lst)
  (foldl (lambda (x res) (+ x (* 2 res))) 0 lst))

(define (remove-leading-zeros lst)
  (foldl (lambda (x res)
     (cond
      [(and (= 0 (length res)) (equal? 0 x)) res]
      [else (append res (list x))])) '() lst))

(define (count-zeros lst)
  (foldl (lambda (x res)
    (cond
      [(equal? 0 x) (+ 1 res)]
      [else res])) 0 (remove-leading-zeros lst)))

(define (group-consecutive lst)
  (foldl (lambda (x res)
     (cond
      [(and (< 0 (length res)) (equal? x (first (last res))))
       (append (drop-right res 1) (list (cons x (last res))))]
      [else (append res (list (list x)))])) '() lst))

(define (encode-with-lengths lst)
  (map (lambda (x) (length x)) (group-consecutive (remove-leading-zeros lst))))

(define (decode-with-lengths lst)
  (apply append
     (map (lambda (x) (make-list (list-ref lst x)
        (cond
          [(even? x) 1]
          [else 0]))) (range (length lst)))))
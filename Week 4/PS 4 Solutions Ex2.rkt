#lang racket
(require redex)
(define (pairs lst)
  (apply append (map (lambda (x)
     (map (lambda (y) (cons (list-ref lst x) y))
         (list-tail lst (+ x 1)))) (range (length lst)))))

(define (splits lst)
  (map (lambda (x)
     (list (take lst x) (list-tail lst x))) (range (+ 1 (length lst)))))

(define (max-product lst)
  (foldl (lambda (x res)
    (cond
      [(empty? res) x]
      [(< (* (car res) (cdr res)) (* (car x) (cdr x))) x]
      [else res])) '() (pairs lst)))

(define (max-binary-op op lst)
  (foldl (lambda (x res)
    (cond
      [(empty? res) x]
      [(< (op (car res) (cdr res)) (op (car x) (cdr x))) x]
      ; Additional check for operation where order of arguments matters,
      ; like subtraction
      [(< (op (car res) (cdr res)) (op (cdr x) (car x))) (cons (cdr x) (car x))]
      [else res])) '() (pairs lst)))

; Special function for getting all possible combinations.
; Notice: we are using (append y (list x)) [instead of simply (cons x y)]
; intensionally because we will obtain result in an order provided by user.
(define (all-combinations lst)
  (foldl (lambda (x res)
        (append res (map (lambda (y)
           (append y (list x))) res))) '(()) lst))

(define (combinations lst n)
  (filter (lambda (x) (= n (length x)))
     (all-combinations lst)))


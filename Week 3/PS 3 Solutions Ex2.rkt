#lang racket
(require redex)
(define employees
  '(("John" "Malkovich" . 29)
    ("Anna" "Petrova" . 22)
    ("Ivan" "Ivanov" . 23)
    ("Anna" "Karenina" . 40)))

(define (fullname emploee)
  (cons (car emploee) (cadr emploee)))

(define (get-annas lst)
  (filter (lambda (x) (equal? (car x) "Anna")) lst))

(define (employees-over-25 lst)
  (map fullname (filter (lambda (x) (< 25 (cddr x))) lst)))
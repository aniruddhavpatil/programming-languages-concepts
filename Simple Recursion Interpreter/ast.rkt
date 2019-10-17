#lang racket
(require eopl/eopl)
(provide (all-defined-out))

(define-datatype Ast Ast?
  [num (n number?)]
  [bool (b boolean?)]
  [id (s symbol?)]
  [primApp (op op?) (rands (list-of Ast?))]
  [assume (binds (list-of bind?)) (exp Ast?)]
  [assume& (binds (list-of bind?)) (exp Ast?)]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [function (formals (list-of id?)) (exp Ast?)]
  [applyf (fid Ast?) (rands (list-of Ast?))]
  [assume* (binds  (list-of bind?)) (exp Ast?)]
  [trace-assume (binds (list-of bind?)) (exp Ast?)])

(define mk-bind (lambda(s a)(list s a)))
(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

(define keywords '(function assume  ifte))
(define id? (and/c symbol? (lambda(s) (not (memq s keywords)))))

(define exp2
  (lambda (ls)
    (cond
    [(equal? (length ls) 1) (first ls)]
    [(equal? (length ls) 2) (expt (first ls) (second ls))]
    [(>= (length ls) 3) (exp2 (cons (expt (first ls) (second ls)) (rest (rest ls))))])))

(define opTable (list (list '+ + 'n)
                      (list '- - 'n)
                      (list '* * 'n)
                      (list '/ / 'n)
                      (list 'IsZero? zero? 1)
                      (list '^ exp2 'n)))

(define lookup-op
   (lambda (sym)
     (let ((vals (filter (lambda(u)(equal? (car u) sym)) opTable)))
             (if (not (null? vals)) (rest (car vals)) null))))

(define op?
   (lambda (sym)
     (not (null? (lookup-op sym)))))

(define op
 (lambda (s)
   (first (lookup-op s))))
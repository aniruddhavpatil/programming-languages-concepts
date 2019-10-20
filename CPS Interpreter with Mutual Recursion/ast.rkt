#lang racket
(provide (all-defined-out))
(require eopl)

(define-datatype Ast Ast?
  [num (n number?)]
  [bool (b boolean?)]
  [id (s symbol?)]
  [primApp (op IsOp?) (rands (list-of Ast?))]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [assume (binds (list-of bind?)) (exp Ast?)] 
  [function (formals (list-of id?)) (exp Ast?)]
  [applyf (fid Ast?) (rands (list-of Ast?))]
  [assume* (binds  (list-of bind?)) (exp Ast?)]
  [setRef (var Ast?) (val Ast?)]
  [seq (exps (list-of Ast?))]
  [break (exp Ast?)]
  [sstring (s string?)]
  [try (exp Ast?) (id  Ast?)(handler Ast?)]
  [throw (id Ast?)]
  [abort (id Ast?)])

(define mk-bind (lambda(s a)(list s a)))
(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

(define keywords '(function assume  ifte))
(define id? (and/c symbol? (lambda(s) (not (memq s keywords)))))

(define opTable (list (list '+ + 'n)
                      (list '- - 'n)
                      (list '* * 'n)
                      (list '/ / 'n)
                      (list 'IsZero? zero? 1)
                      (list 'IsEqual? equal? 2)
                      (list '<? < 2)
                      (list '> > 2)))

(define lookupOp
   (lambda (sym)
     (let ((vals (filter 
                    (lambda(u)(equal? (car u) sym)) 
                    opTable)))

             (if (not (null? vals))
                 (rest (car vals))
                 null))))

(define IsOp?
   (lambda (sym)
     (not (null? (lookupOp sym)))))

(define op 
 (lambda (s)
   (first (lookupOp s))))
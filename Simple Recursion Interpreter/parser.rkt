#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt")

(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]  
      [(list? ls) 
         (let ((head (first ls))(tail (rest ls)))
          (cond
                [(op? head)  (letrec ((opInfo (lookup-op head))
                                        (arity (second opInfo)))                                 
                                  (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail)))) 
                                      (primApp head (map parse tail))
                                      (error "Number of arguments do not match with arity of operator")))]
                [(equal? 'assume head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume ast-binds ast-body)))]
                [(equal? 'assume* head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume*")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume* ast-binds ast-body)))]
                [(equal? 'trace-assume head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume*")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (trace-assume ast-binds ast-body)))]
                [(equal? 'assume& head) (if (not (equal? 2 (length tail)))
                                        (error "parse :" "Bad syntax for assume&")
                                        (letrec ((ast-binds (map (lambda(u) (mk-bind (first u) (parse (second u)))) (le (first tail))))
                                                (ast-body  (parse (second tail))))
                                         (assume& ast-binds ast-body)))]
                [(equal? 'ifte head) (if (not (equal? 3 (length tail))) 
                                              (error "parse :" "Bad syntax for ifte") 
                                           (letrec ((test (parse (first tail))) 
                                                    (then (parse (second tail)))
                                                    (else (parse (third tail))))
                                      (ifte test then else)))]

	        [(equal? 'function head) (if (not (equal? 2 (length tail))) 
                                              (error "parse :" "Bad syntax for function")
                                              (letrec ((formals (first tail))  (body (second tail)))
                                                   (function formals (parse body))))]
                [(id? head)        (applyf (parse head) (map parse tail))]
                (else              (error "parse :" "bad type"))))]
       (else (cond
                [(number? ls)    (num ls)]
                [(boolean? ls)  (bool ls)]
                [(id? ls)         (id ls)]
                (else (error "bad input to parser"))))
      
  )))

(define le
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons (car ls) (le (replace (caar ls) (cadar ls) (cdr ls))))])))

(define (deep-map f l)
  (let deep ((x l))
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x)))))

(define replace
  (lambda (sym val ls)
    (cond
      [(null? ls) '()]
      [else (cons (deep-map (lambda (u) (cond
                               [(list? u) (replace sym val u)]
                               [(equal? sym u) val]
                               [else u])) (car ls)) (replace sym val (cdr ls)))])))
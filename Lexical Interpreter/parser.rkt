#lang racket
(provide (all-defined-out))
(require "ast.rkt")

(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]
      [(list? ls)
       (let ((head (first ls))(tail (rest ls)))
          (cond
            [(number? head) (num head)]
            [(boolean? head) (bool head)]
            [(op? head)  (letrec ((opInfo (lookup-op head)) (arity (second opInfo)))
                           (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail))))
                               (primApp head (map parse tail))
                               (error "Number of arguments do not match with arity of operator")))]
            [(equal? 'assume& head) (if (not (equal? 2 (length tail)))
                                        (error "parse :" "Bad syntax for assume&")
                                        (letrec ((ast-binds (map (lambda(u) (mk-bind (first u) (parse (second u)))) (mylet* (first tail))))
                                                (ast-body  (parse (second tail))))
                                         (assume& ast-binds ast-body)))]
            [(equal? 'assume head) (if (not (equal? 2 (length tail)))
                                       (error "parse :" "Bad syntax for assume")
                                       (letrec ((ast-binds (map (lambda(u) (mk-bind (first u) (parse (second u)))) (first tail)))
                                                (ast-body  (parse (second tail))))
                                         (assume ast-binds ast-body)))]
            [(equal? 'ifte head) (if (not (equal? 3 (length tail)))
                                     (error "parse :" "Bad syntax for ifte")
                                     (letrec ((test (parse (first tail)))
                                              (then (parse (second tail)))
                                              (else (parse (third tail))))
                                       (ifte test then else)))]
            [(and (symbol? head) (zero? (length tail))) (id head)]
            [else (error "parse :" "bad type")]))]
       (else (parse (list ls))))))

(define mylet*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (cons (car ls) (mylet* (replace (caar ls) (cadar ls) (cdr ls))))])))

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

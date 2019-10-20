#lang racket
(provide (all-defined-out))
(require "env.rkt" "ast.rkt")
 
(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]
      [(list? ls) 
         (let ((head (first ls))(tail (rest ls)))
          (cond
               [(IsOp? head)  (letrec ((opInfo (lookupOp head))
                                        (arity (second opInfo)))
                                  (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail)))) 
                                      (primApp head (map parse tail))
                                      (error "Number of arguments do not match with arity of operator")))]
     [(equal? 'setRef head) (if (equal? (length tail) 2) 
                                         (letrec ((var-exp (car tail)) (val-exp (cadr tail)))
                                             (setRef (parse var-exp) (parse val-exp)))
                                         (error "parse :" "bad syntax for setRef"))]
                  
     [(equal? 'seq head)   (letrec ((a (map parse tail)))
                                   (seq a))]

                  [(equal? 'break head) (if (equal? (length tail) 1) 
                                         (letrec ((exp (car tail)))
                                             (break (parse exp)))
                                         (error "parse :" "bad syntax for break"))]
                  [(and (string? head)(null? tail))  (sstring head)]
                  [(equal? 'abort head) (if (equal? (length tail) 1)  
                                         (letrec ((exp (car tail)))
                                             (abort (parse exp)))
                                         (error "parse :" "bad syntax for abort"))]
                     
                  [(equal? 'throw head) (if (equal? (length tail) 1) 
                                         (letrec ((exp (car tail)))
                                             (throw (parse exp)))
                                         (error "parse :" "bad syntax for throw"))]
                     
                  [(equal? 'try head) (if (equal? (length tail) 3) 
                                         (letrec ((exp (parse (car tail))) 
                                                  (ids (parse (cadr tail)))
                                                  (handler (parse (caddr tail))))
                                                               (try exp ids handler))
                                           (error "parse :" "bad syntax for try"))]

                                       

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


 
	         [(equal? 'function head) (if (not (equal? 2 (length tail))) 
                                              (error "parse :" "Bad syntax for function")
                                              (letrec ((formals (first tail))  (body (second tail)))
                                                   (function formals (parse body))))]
                 [(id? head)        (applyf (parse head) (map parse tail))]

                 [(equal? 'ifte head) (if (not (equal? 3 (length tail))) 
                                              (error "parse :" "Bad syntax for ifte") 
                                           (letrec ((test (parse (first tail))) 
                                                    (then (parse (second tail)))
                                                    (else (parse (third tail))))
                                      (ifte test then else)))]

                 (else              (error "parse :" "bad type"))))]
       (else  (cond
                [(number? ls)    (num ls)]
                [(boolean? ls)   (bool ls)]
                [(id? ls)        (id ls)]
                [(string? ls)    (sstring ls)]
                (else (error "bad input to parser"))))
      
  )))
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt" "env.rkt" "parser.rkt")

(define trace #f)

(define apply-closure (lambda (c rands envOuter [trace #f])
(cases functionHandle c
   [rec-closure (formals body env rec-tuples)
            (letrec ((args (map (lambda (x)(expressible->denotable (eval-ast x envOuter trace))) rands))
                  (rr (map (lambda (x) (mk-rec-tuple (first x) (second x) rec-tuples)) rec-tuples)))
             (cond
               [(equal? #t trace) (printf "> ") (map (lambda (u) (printf " (~a ~a) " (first u) args)) rr) (printf "\n") (letrec ((ans (eval-ast body (extended-env rr (extended-env (mk-tuples formals args) env)) #t))) (printf "< ~a\n" ans) ans)]
               [else (eval-ast body (extended-env rr (extended-env (mk-tuples formals args) env)) trace)]))]
   [rec-closure-v (formals body env)
            (let ((args (map (lambda (x)(expressible->denotable (eval-ast x envOuter trace))) rands)) (en (vector-ref env 0)))
              (cond
               [(equal? #t trace) (letrec ((ans (eval-ast body (extended-env (mk-tuples formals args) en) trace))) (printf "< ~a\n" ans) ans)]
               [else (eval-ast body (extended-env (mk-tuples formals args) en) trace)]))]
   [closure (formals body env)
           (let ((args (map (lambda (x)(expressible->denotable (eval-ast x envOuter trace))) rands)))
            (eval-ast body  (extended-env (mk-tuples formals args) env) trace))]
   (else (error 'not_a_function_handle)))))

(define eval-ast
  (lambda (ast env [trace #f])
    (cases Ast ast
      [num (n) n]
      [bool(b) b]
      [id (s) (denotable->expressible (lookupEnv s env))]
      [ifte (test then else) (let ((b (eval-ast test env trace)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env trace)
                                                   (eval-ast else env trace))]
                                  (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
      [assume (binds body) (let ((tpls (map  (lambda(u)
                                               (mk-tuple  (first u)  (expressible->denotable (eval-ast (second u) env trace))))
                                               binds))) ;evaluate asts to get values to be bound to identifiers
                                 (eval-ast body (extended-env tpls env) trace))] ;evaluate the body in the extended envirnment
      [assume* (binds body) (letrec ((tpls (map  (lambda(u)
                                                     (mk-tuple
                                                              (first u)
                                                              (expressible->denotable (eval-ast (second u) env trace))))
                                               binds))
                                     (rec-tup (filter (lambda (u) (functionHandle? (denotable->expressible (second u)))) tpls))
                                     (rec-tuples (map (lambda (u) (mk-rec-tuple (first u) (second u) rec-tup)) tpls))) ;evaluate asts to get values to be bound to identifiers
                                   (eval-ast body (extended-env rec-tuples env) trace))]
      [assume-v (binds body) (letrec ((tpls (map  (lambda(u)
                                                     (mk-tuple
                                                              (first u)
                                                              (expressible->denotable (eval-ast (second u) env trace))))
                                               binds))
                                     (rec-tup (filter (lambda (u) (functionHandle? (denotable->expressible (second u)))) tpls))
                                     (rec-tuples (map (lambda (u) (mk-rec-tuple-v (first u) (second u))) tpls))) ;evaluate asts to get values to be bound to identifiers
                                   (eval-ast body (extended-env rec-tuples env) trace))]
      [trace-assume (binds body) (letrec ((tpls (map  (lambda(u)
                                                     (mk-tuple
                                                              (first u)
                                                              (expressible->denotable (eval-ast (second u) env trace))))
                                               binds))
                                          (rec-tup (filter (lambda (u) (functionHandle? (denotable->expressible (second u)))) tpls))
                                          (rec-tuples (map (lambda (u) (mk-rec-tuple (first u) (second u) rec-tup)) tpls)))
                                 (eval-ast body (extended-env rec-tuples env) #t))]
      [assume& (binds body) (let ((tpls (map (lambda(u) (mk-tuple (first u) (expressible->denotable (eval-ast (second u) env trace)))) binds)))
                                  (eval-ast body (extended-env tpls env) trace))]
      [primApp (s rands) (letrec ((proc (op s))                                     ;get the operator procedure
                                  (args (map (lambda(u)(eval-ast u env trace)) rands)))   ;evaluate operands to get actual arguments
                           (if (equal? proc exptN)
                               (exptN args)
                           (apply proc args)))]
      [setRef (var val) (*setRef (lookupEnv
                                   (cases Ast var
                                         [id (s) s]
                                         (else (error "bad id type")))
                                     env)
                               (eval-ast val env trace))]
      [seq (exps) (letrec ([args (map (lambda(u)(eval-ast u env trace)) exps)]
                           [n    (length args)])
                     (list-ref args (- n 1)))]
      [function (formals body) (closure formals body env)]
      [applyf (fid rands) (letrec ((c (eval-ast fid env trace)))
                            (let ((ans (apply-closure c rands env trace)))
                              (cond
                                [(equal? trace #t) ans]
                                [else ans])))]
      )))

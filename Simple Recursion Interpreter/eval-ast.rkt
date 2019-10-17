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
   [rec-closure (formals body env)
            (let ((args (map (lambda (x)(eval-ast x envOuter trace)) rands)) (e (vector-ref env 0)) (fid (vector-ref env 1)))
             (cond
               [(equal? #t trace) (printf "> (~a ~a)\n" fid args) (eval-ast body (extended-env (list (mk-tuple fid c)) (extended-env (mk-tuples formals args) e)) trace)]
               [else (eval-ast body (extended-env (list (mk-tuple fid c)) (extended-env (mk-tuples formals args) e)) trace)]))]
   [closure (formals body env)
           (let ((args (map (lambda (x)(eval-ast x envOuter trace)) rands)))
            (eval-ast body  (extended-env (mk-tuples formals args) env) trace))]
   (else (error 'not_a_function_handle)))))

(define eval-ast
  (lambda (ast env [trace #f])
    (cases Ast ast
      [num (n) n]
      [bool(b) b]
      [id (s) (denotable->expressible(lookupEnv s env))]
      [ifte (test then else) (let ((b (eval-ast test env trace)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env trace)
                                                   (eval-ast else env trace))]
                                  (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
      [assume (binds body)   (let ((tpls (map  (lambda(u)
                                               (mk-tuple  (first u)  (expressible->denotable (eval-ast (second u) env trace))))
                                               binds)))
                                 (eval-ast body (extended-env tpls env) trace))]
      [assume* (binds body) (let ((tpls (map  (lambda(u)
                                                     (mk-rec-tuple
                                                              (first u)
                                                              (expressible->denotable (eval-ast (second u) env trace))))
                                               binds)))
                                   (eval-ast body (extended-env tpls env) trace))]
      [trace-assume (binds body) (let ((tpls (map  (lambda(u)
                                                     (cases Ast (second u)
                                                       [function (formals e) (printf "output =>\n") (mk-rec-tuple
                                                              (first u)
                                                              (expressible->denotable (eval-ast (second u) env trace)))]
                                                       [else (mk-tuple (first u) (expressible->denotable (eval-ast (second u) env trace)))])
                                                     )
                                               binds)))
                                 (eval-ast body (extended-env tpls env) #t))]
      [assume& (binds body) (let ((tpls (map (lambda(u) (mk-tuple (first u) (expressible->denotable (eval-ast (second u) env trace)))) binds)))
                                  (eval-ast body (extended-env tpls env) trace))]
      [primApp (s rands) (letrec ((proc   (op s))
                                  (args   (map (lambda(u)(eval-ast u env trace)) rands)))
                           (apply proc args))]
      [function (formals body) (closure formals body env)]
      [applyf (fid rands) (letrec ((c (eval-ast fid env trace)))
                            (let ((ans (apply-closure c rands env trace)))
                              (cond
                                [(equal? trace #t) (printf "< ~a\n" ans) ans]
                                [else ans])))]
      )))

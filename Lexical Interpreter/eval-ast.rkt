#lang racket
(require eopl/eopl "ast.rkt" "env.rkt")
(provide (all-defined-out))

(define eval-ast
  (lambda (ast env)
    (cases Ast ast
      [num (n) n]
      [bool(b) b]
      [id (s) (denotable->expressible(lookup-env s env))]
      [ifte (test then else) (let ((b (eval-ast test env)))
                                  (cond
                                    [(boolean? b) (if b (eval-ast then env) (eval-ast else env))]
                                    (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
      [assume& (binds body) (let ((tpls (map (lambda(u) (mk-tuple (first u) (expressible->denotable (eval-ast (second u) env)))) binds)))
                                  (eval-ast body (extended-env tpls env)))]
      [assume (binds body) (let ((tpls (map (lambda(u) (mk-tuple (first u) (expressible->denotable (eval-ast (second u) env)))) binds)))
                                  (eval-ast body (extended-env tpls env)))]
      [primApp (s rands) (letrec ((proc (op s)) (args (map (lambda(u)(eval-ast u env)) rands)))
                           (cond
                             [(equal? proc exptN) (exptN args)]
                             (else (apply proc args))
                             ))])))

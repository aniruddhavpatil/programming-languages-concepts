#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt")

(define-datatype functionHandle functionHandle?
  (closure (formals (list-of symbol?)) (body Ast?) (env Env?))
  (rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?))))

(define closure? 
          (lambda(c) (cases functionHandle c
                             [closure (formals body env ) #t]
                             (else #f))))

(define expressible? (or/c number? boolean? functionHandle?))
(define ans? expressible?)
(define denotable? expressible?)
(define denotable->expressible (lambda(thing) thing))
(define expressible->denotable (lambda(thing) thing))

(define tuple? (lambda(x)
                    (and (list? x) 
                         (symbol? (first x)) 
                         (denotable? (second x)))))

(define mk-tuple (lambda (x y) (list x y)))

(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env? )])

(define lookupEnv (lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookupEnv:empty-env" "unbound identifier")]
                           [extended-env (tuples outer-env)
                                            (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                                 (if (not tpl) 
                                                     (lookupEnv var outer-env)
                                                     (second tpl)))])))

(define mk-tuples (lambda(ids vals) 
                   (if (not (equal? (length ids) (length vals)))
                            (error "mk-tuples: " "mismatch in environment tuples")
                       (map mk-tuple ids vals))))

(define mk-rec-tuple (lambda (x y renv) 
                       (mk-tuple x (if (functionHandle? y)
                                   (cases functionHandle y
                                        [closure (formals body env) 
                                            (rec-closure formals body env renv)]
                                        (else "unknown closure type"))
                                 y))))

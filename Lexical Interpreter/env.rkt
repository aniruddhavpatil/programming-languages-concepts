#lang racket
(require eopl)
(provide (all-defined-out))
(define expressible? (or/c number? boolean?))
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
        [extended-env (tuples (list-of tuple?)) (outer-env Env? )]
)

(define lookup-env (lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookup-env:empty-env" "unbound identifier")]
                           [extended-env (tuples outer-env) (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                                              (if (not tpl)
                                                                  (lookup-env var outer-env)
                                                                  (second tpl)))])))

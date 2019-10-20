#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt")

(define applyk (lambda (k v) (apply k (list v))))

(define *resume* 'undefined)
(define *val* 'undefined)
(define get-resume (lambda () (*resume*)))
(define set-resume (lambda(k) (set! *resume* k)))
(define get-val (lambda () (*val*)))
(define set-val (lambda(v) (set! *val* v)))
(define resume! (lambda (val)(applyk *resume* val)))
(define resume (lambda ()(begin (applyk *resume* *val*))))

(define-datatype functionHandle functionHandle?
  [closure (formals (list-of symbol?)) (body Ast?) (env Env?)]
  [rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?))])
 

(define expressible? (or/c number? boolean? functionHandle?)) 
(define ans? expressible?)
(define reference? integer?)
(define denotable? reference?)
(define storable? expressible?)
(define denotable->expressible (lambda(thing) (*deRef thing)))
(define expressible->denotable (lambda(thing) (*newRef thing)))
 
(define closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [closure (formals body env) #t]
      (else #f))
   #f)))

(define rec-closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [rec-closure (formals body env tpls) #t]
      (else #f))
   #f)))
 
(define tuple? (lambda(x)
                    (and (list? x) 
                         (symbol? (first x)) 
                         (denotable? (second x)))))

(define mk-tuple (lambda (x y) (list x y)))

 (define mk-rec-tuple (lambda (x y rec-env) 
                          (mk-tuple x
                             (let ((y1 (denotable->expressible y)))
                                (if (functionHandle? y1)
                                    (cases functionHandle y1
                                      [closure (formals body env) 
                                           (expressible->denotable (rec-closure formals body env rec-env))]
                                      (else "unknown closure type"))
                                     y)))))

(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env?)]
        )

(define lookupEnv/k(
                   lambda (var env k)                    
                          (cases Env env
                           [empty-env () (error (format "lookupEnv/k unbound identifier: ~a" var))]    
                           [extended-env (tuples outer-env)
                                     (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                          (if (not tpl)
                                               (lookupEnv/k var outer-env k)
                                               (applyk k (second tpl))))]
                            (else (error (format "lookupEnv/k: bad environment ~a" env)))
                     )))

(define mk-tuples (lambda(ids vals) 
                   (if (not (equal? (length ids) (length vals)))
                            (error "mk-tuples: " "mismatch in environment tuples")
                       (map mk-tuple ids vals))))

(define store '())

(define init-store (lambda()(set! store '())))

(define *newRef (lambda(v)(begin 
                           (set! store (append store (list v))) 
                           (- (length store) 1))))

(define *deRef (lambda(l) (let ((len (length store)))
                           (if (or (zero? len) (> l len)) 
                               (error "store- deRef: out of bound")
                               (list-ref store l)))))

(define *setRef/k
         (lambda (l v k)
		 (letrec ((setRef-inner/k 
                             (lambda (ls n k)
                               (if (zero? n) 
                                   (k (cons v (cdr ls)))
                                   (if (null? ls)
                                       (error (format "*setRef/k : out of bound- short of ~a length" n))
                                       (setRef-inner/k 
                                                   (cdr ls) 
                                                   (- n 1)
                                                   (lambda (v) 
                                                           (applyk k (cons (car ls) v)))))))))
                          (setRef-inner/k store l (lambda(w) (applyk k (set! store w)))))))
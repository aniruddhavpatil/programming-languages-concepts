#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt")

(define-datatype functionHandle functionHandle?
  (closure (formals (list-of symbol?)) (body Ast?) (env Env?))
  (rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?)))
  (rec-closure-v (formals (list-of symbol?)) (body Ast?) (env vector?)))

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

(define expressible? (or/c number? boolean? functionHandle?)) 
(define ans? expressible?)
(define reference? integer?)
(define denotable? reference?)
(define storable? expressible?)
(define denotable->expressible (lambda(thing) (*deRef thing)))
(define expressible->denotable (lambda(thing) (*newRef thing)))

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

(define mk-rec-tuple (lambda (x y rec-env) 
                          (mk-tuple x
                             (let ((y1 (denotable->expressible y)))
                                (if (functionHandle? y1)
                                    (cases functionHandle y1
                                      [closure (formals body env) 
                                           (expressible->denotable (rec-closure formals body env rec-env))]
                                      (else "unknown closure type"))
                                     y)))))

(define mk-rec-tuple-v (lambda (x y) (mk-tuple x
                                        (let ((y1 (denotable->expressible y)))
                                          (if (functionHandle? y1)
                                            (cases functionHandle y1
                                                 [closure (formals body env)
                                                      (letrec ((v (make-vector 1))
                                                               (c (expressible->denotable (rec-closure-v formals body v))))
                                                            
                                                          (begin 
                                                                 (vector-set! v 0 (extended-env
                                                                                      (list (mk-tuple x c)) 
                                                                                       env))
                                                                  c))]
                                                  (else (error "unknown closure type")))
                                            y)))))

(define store '())

(define init-store (lambda()(set! store '())))

;; newRef : storable? -> reference?
;; returns a new reference with stored value v
(define *newRef (lambda(v)(begin 
                           (set! store (append store (list v))) 
                           (- (length store) 1))))

     

;; deRef : denotable? -> storable?
;; returns the stored value for a given reference

(define *deRef (lambda(l) (let ((len (length store)))
                           (if (or (zero? len) (> l len)) 
                               (error "store- deRef: out of bound")
                               (list-ref store l)))))

;; setRef : denotable? storable? -> void
;; changes the value stored at location referenced by l to v
;; iterates through the store to point to correct index
;; creates a new store with updated value at location l

(define *setRef (lambda (l v)
		 (letrec ((setRef-inner 
                           (lambda (ls n)
			     (if (zero? n) 
                                 (cons v (cdr ls))
                                 (if (null? ls)
                                     (error (list "*setRef : out of bound" n))
                                     (cons (car ls) 
                                           (setRef-inner 
                                                   (cdr ls) 
                                                   (- n 1))))))))
(set! store (setRef-inner store l)))))
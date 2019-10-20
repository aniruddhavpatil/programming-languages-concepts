#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "env.rkt" "ast.rkt" "parse.rkt")

(define map/k (lambda(f ls k)
              (if (null? ls) (applyk k '())
                  (f (car ls) (lambda(v)
                                (map/k f (cdr ls) (lambda (w)(k (cons v w)))))))))

(define call-by-value/k (lambda (rands env k)
                          (map/k
                                 (lambda (x k)
                                         (eval-ast/k x env  (lambda(v) (k (expressible->denotable v)))))
                                 rands
				 k)))

(define call-by-name (lambda (rands env k)
                             (map
                                 (lambda (x)
                                   (cases Ast x

                                    [id (s) (lookupEnv/k s env k)]
                                    (else (expressible->denotable
                                             (lambda () (eval-ast/k x env k))))))
                                 rands)))


(define apply-closure/k (lambda (c rands envOuter k)
(cases functionHandle c
  [rec-closure (formals body env r)
            (let ((args  (call-by-name rands envOuter k))
                  (rr    (map (lambda (x) (mk-rec-tuple (first x) (second x) r)) r)))

             (eval-ast/k body (extended-env rr
                                        (extended-env (mk-tuples formals args) env)) k))]

   [closure (formals body env)
           (let ((args (call-by-name rands envOuter k)))
            (eval-ast/k body  (extended-env (mk-tuples formals args) env) k))]
   (else (error (format "not_a_function_handle ~a" c))))))


(define eval-ast/k
  (lambda (ast env k)
    (cases Ast ast

      [num (n)  (applyk k n)]

      [bool(b) (applyk k b)]

      [function (formals body)    (applyk k (closure formals body env))]

     [ifte (test then else) (eval-ast/k test
                                        env
				        (lambda (b)
                                         (if (boolean? b)
					     (if b
					       (eval-ast/k then env k)
                                               (eval-ast/k else env k))
                                             (error (format "eval-ast/k - ifte:test condition must be boolean instead of ~a" b)))))]

     [setRef (var val) (eval-ast/k val
                                   env
                                   (lambda (v)
                                         (lookupEnv/k  (cases Ast var  [id (s) s] (else (error  "setRef: bad id type")))
					                env
                                                        (lambda (l)
                                                              (*setRef/k l  v k)))))]
     [id (s) (lookupEnv/k s
                        env
			  (lambda(v) (applyk k (denotable->expressible v))))]
          
     [primApp (s rands) (letrec ([proc   (op s)])
                        (map/k (lambda(u k)
                                      (eval-ast/k u env k))
                               rands
                               (lambda(args)
                                 (letrec ((argsT (map (lambda (u) (if (procedure? u) (u) u)) args)) (app (applyk k (apply proc argsT))))
                                   (if (procedure? app) (app) app)))))]
    [applyf (fid rands) (eval-ast/k fid
                                    env
                                    (lambda(c)
                                           (apply-closure/k c rands env k)) )]
    [assume (binds body)
	    (map/k  (lambda(u k)
		      (eval-ast/k  (second u)
			  	   env
				   (lambda(v)
				        (applyk k (mk-tuple (first u) (expressible->denotable  v))))))
	             binds
	             (lambda (tpls)
             	              (eval-ast/k body
			                 (extended-env tpls env)
			                  k)))]
     [assume* (binds body)
	 (map/k  (lambda(u k) (eval-ast/k (second u)
				      	  env
					  (lambda(v) (applyk k (mk-tuple (first u)  (expressible->denotable v))))))
	         binds
	         (lambda (tplsAll)
	          (letrec (
			  [closure-tuples (filter (lambda(u)
						    (functionHandle?  (denotable->expressible (second u))))
                                                  tplsAll)])
                           (map/k  (lambda(u k)
                                          (applyk k (mk-rec-tuple (first u) (second u) closure-tuples)))
                                   tplsAll
			           (lambda (recTplsAll)
			                   (eval-ast/k body
				                       (extended-env recTplsAll env)
				                       k))))))]
  [seq (exps) (map/k (lambda(u k)(eval-ast/k u env k))
                      exps
                      (lambda(args)
                        (applyk k (list-ref args (- (length args) 1)))))]

   [sstring(s) (applyk k s)]


   [throw (exp)    (eval-ast/k exp
                               env
                               (error "Error"))]

  [try (exp err-id handler) (cases Ast err-id
                               [id (s)
                                  (eval-ast/k
                                          exp
                                          env
                                          k
                                          (lambda(v)
                                                (eval-ast/k handler
                                                             (extended-env (list (mk-tuple
                                                                                      s
                                                                                      (expressible->denotable v)))  env))))]
                               (else (error "bad error id")))]



   [abort  (exp)          (eval-ast/k exp
                                       env
                                       (error "Error"))]


  [break (exp)     (begin
                      (eval-ast/k exp
                                  env
                                  (lambda(v)
                                         (begin
                                              (printf "In break continuation- value: ~s \n" v)
                                              (set-resume k)
                                              (set-val v)
                                               v))))]

 )
  )
 )

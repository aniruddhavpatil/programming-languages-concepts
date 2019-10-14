#lang racket
(provide (all-defined-out))
(require eopl/eopl)
(require test-engine/racket-tests)

(define-datatype Ast Ast?
  [num (n number?)]
  [primApp (op IsOp?) (rands (list-of Ast?))])

(define exptN
  (lambda(ls)
  (cond
    [(equal? (length ls) 1) (first ls)]
    [(equal? (length ls) 2) (expt (first ls) (second ls))]
    [(>= (length ls) 3) (exptN (cons (expt (first ls) (second ls)) (rest (rest ls))))])))

(define opTable (list (list 'plus + 'n '+)
                      (list 'minus - 'n '-)
                      (list 'mul * 'n '*)
                      (list 'div / 'n '/)
                      (list 'pow exptN 'n '^)))


(define lookupOp
   (lambda (sym)
     (let (
           (vals (filter (lambda(u) (equal? (car u) sym)) opTable)))
             (cond
               [(not (null? vals)) (rest (car vals))]
               (else  null)))))

(define IsOp?
   (lambda (sym)
     (not (null? (lookupOp sym)))))

(define lookupName
  (lambda (sym)
     (let (
           (vals (filter (lambda(u) (equal? (fourth u) sym)) opTable)))
             (cond
               [(not (null? vals)) (first (first vals))]
               (else  null)))))

(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Input in bad format:(null)")] 
      [(list? ls)
         (let ((head (car ls)) (tail (cdr ls)))
          (cond
                [(null? head) (error "Input in bad format:(null)")]
                [(number? head) (num head)]
                ;[(null? (lookupName head)) (error "Bad operator type")]
                [(and (and (null? (lookupName (car tail))) (not (number? (car tail)))) (not (list? (car tail)))) (error "parse: unrecognized type" )]
                [(not (null? (lookupName head))) (primApp (lookupName head) (map parse tail))]
                ;[(and (null? (lookupName head)) (not (number? head))) (error "parse: unrecognized type" )]
                [else (error "Bad operator type")]))]
       (else (parse (list ls)))
         
  )))


(define ans? number?)

(define eval
  (lambda (ast)
    (cases Ast ast 
      [num (n)  n]
      [primApp (op rands) (letrec ((opInfo (lookupOp op))
                                   (proc (first opInfo))
                                   (arity (second opInfo))
                                   (args (map eval rands)))


                              (cond
                                [(cond [(equal? proc exptN) (exptN args)]
                                      (else (apply proc args)))]
                                ;[(or (equal? arity 'n) (and (number? arity) (equal? arity (length args)))) (apply proc args)]
                                  (error "eval: primApp: " "wrong number of arguments")))] 
      )))

;(require rackunit)
;(require test-engine/racket-tests)
;(check-equal? 5 (eval (parse '(+ 2 (/ 9 (* 3 (- 4 3)))))))
;(check-equal? 11 (eval (parse '(+ 3 5 (/ 9 3)))))
;(check-equal?  7 (eval (primApp  'plus (list (num 5)  (primApp 'div (list (num 20) (num 2) (num 5) ))))))
;(check-equal? 8 (eval (parse '(+ 5 (/ 9 3)))))
;(check-equal? 8 (eval (parse '(^ 2 (* 3 (- 4 3))))))
;(check-equal? 36 (eval (parse '(^ (- (* 3 3) 3) (* 2 (/ 7 7) (- 10 9)))))) 
;(check-error (eval (parse '(+ 2 (/ 9 (*3 (- 4 3)))))) "Bad operator type")
;(check-error (eval (parse '())) "Input in bad format:(null)")
;(check-error (eval (parse '(+ #f 3))) "parse: unrecognized type")
#lang typed/racket

;Authors: Anthony Teciorowski, Scott Hufschmidt

(require typed/rackunit)

(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]))
(define-type ExprC (U NumC IdC AppC BinopC leq0))
(struct NumC ([n : Real]))
(struct IdC ([s : Symbol]))
(struct AppC ([fun : Symbol] [arg : ExprC]))
(struct BinopC ([op : binop] [l : ExprC] [r : ExprC]))
(struct leq0 ([exp : ExprC]))
(define-type binop (U '+ '- '* '/))

; Determines if symbol is a valid operator
(define (binop? [v : Sexp]) : Boolean
  (if (symbol? v)
      (match v
        ['+ #t]
        ['- #t]
        ['* #t]
        ['/ #t]
        [_ #f])
      (error 'binop? "VVQS given op was not a symbol ~e" v)))

(check-true (binop? '+))
(check-true (binop? '-))
(check-true (binop? '*))
(check-true (binop? '/))
(check-false (binop? 's))
(check-exn (regexp (regexp-quote "VVQS given op was not a symbol"))
           (lambda () (binop? 1)))


; ---------------------------------------------------
; Parser and tests

; the parse function will take in an s-expression and modify it to create an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(list op arg1 arg2)
     (match op
       [(? binop? op) (BinopC (cast op binop) (parse arg1) (parse arg2))]
       [_ (error 'parse "VVQS invalid list input ~e" s)])]
    [_ (error 'parse "VVQS invalid input ~e" s)]))


; Custom comparison function for ExprC instances
(define (exprc-equal? [e1 : ExprC] [e2 : ExprC]) : Boolean
  (match* (e1 e2)
    [((BinopC op1 l1 r1) (BinopC op2 l2 r2)) (and (eq? op1 op2) (exprc-equal? l1 l2) (exprc-equal? r1 r2))]
    [((NumC n1) (NumC n2)) (= n1 n2)]))

(check-true (exprc-equal? (parse '(+ 1 2)) (BinopC '+ (NumC 1) (NumC 2))))
(check-true (exprc-equal? (parse '(* 3 4)) (BinopC '* (NumC 3) (NumC 4))))
(check-true (exprc-equal? (parse '(/ 10 5)) (BinopC '/ (NumC 10) (NumC 5))))
(check-false (exprc-equal? (parse '(/ 10 5)) (BinopC '- (NumC 10) (NumC 5))))
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse 'not-a-list-or-num)))
(check-exn (regexp (regexp-quote "VVQS invalid list input"))
           (lambda () (parse (cast '(l 2 3) Sexp))))



; ---------------------------------------------------
; Interpreter and tests

; Lookup table for binary operators
(define (binop-exec [b : ExprC]) : Real
  (match b
    [(NumC n) n]
    [(BinopC '+ l r) (+ (binop-exec l) (binop-exec r))]
    [(BinopC '- l r) (- (binop-exec l) (binop-exec r))]
    [(BinopC '* l r) (* (binop-exec l) (binop-exec r))]
    [(BinopC '/ l r) (/ (binop-exec l) (binop-exec r))]
    ))

(define (interp [exp : ExprC]) : Real
  (match exp
    [(NumC n) n]
    [(IdC s) (error 'interp "VVQS unbound identifier: ~a" s)]
    [(BinopC op l r) (binop-exec exp)]
    ;[_ (error 'interp "VVQS invalid operator: ~a" exp)]
    ))

(check-equal? (interp (NumC 2)) 2)
(check-equal? (interp (parse '(+ 1 2))) 3)
(check-equal? (interp (parse '(* 3 4))) 12)
(check-equal? (interp (parse '(- 2 1))) 1)
(check-equal? (interp (parse '(/ 3 3))) 1)
(check-exn (regexp (regexp-quote "VVQS unbound identifier:"))
           (lambda () (interp (IdC 'a))))
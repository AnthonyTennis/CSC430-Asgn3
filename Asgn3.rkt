#lang typed/racket

;Authors: Anthony Teciorowski, Scott Hufschmidt

(require typed/rackunit)

(struct FundefC ([name : Sexp] [arg : Sexp] [body : ExprC])
  #:transparent)
(define-type ExprC (U NumC IdC AppC BinopC leq0 FundefC))
(struct NumC ([n : Real])
   #:transparent)
(struct IdC ([s : Symbol])
   #:transparent)
(struct AppC ([arg : ExprC] [val : ExprC])
   #:transparent)
(struct BinopC ([op : binop] [l : ExprC] [r : ExprC])
   #:transparent)
(struct leq0 ([exp : ExprC])
   #:transparent)
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

; test cases for binop?
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
    [(? symbol? s) (IdC s)]
    [(list op arg1 arg2)
     (match op
       [(? binop? op) (BinopC (cast op binop) (parse arg1) (parse arg2))]
       [_ (error 'parse "VVQS invalid list input ~e" s)])]
    [(list f arg) (AppC (parse f) (parse arg))]
    [_ (error 'parse "VVQS invalid input ~e" s)]))


; Custom comparison function for ExprC instances
(define (exprc-equal? [e1 : ExprC] [e2 : ExprC]) : Boolean
  (match* (e1 e2)
    [((BinopC op1 l1 r1) (BinopC op2 l2 r2)) (and (eq? op1 op2) (exprc-equal? l1 l2) (exprc-equal? r1 r2))]
    [((IdC s1) (IdC s2)) (eq? s1 s2)]
    [((FundefC n1 a1 b1) (FundefC n2 a2 b2)) (and (eq? n1 n2) (eq? a1 a2) (exprc-equal? b1 b2))]
    [((NumC n1) (NumC n2)) (= n1 n2)]
    [((AppC a1 a2) (AppC b1 b2)) (and (exprc-equal? a1 b1) (exprc-equal? a2 b2))]))

; test cases for exprc-equal? and parse
(check-true (exprc-equal? (parse '(+ 1 2)) (BinopC '+ (NumC 1) (NumC 2))))
(check-true (exprc-equal? (parse '(* 3 4)) (BinopC '* (NumC 3) (NumC 4))))
(check-true (exprc-equal? (parse '(/ 10 5)) (BinopC '/ (NumC 10) (NumC 5))))
(check-false (exprc-equal? (parse '(/ 10 5)) (BinopC '- (NumC 10) (NumC 5))))
(check-true (exprc-equal? (parse '(f 12)) (AppC (IdC 'f) (NumC 12))))
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse '(1))))
(check-exn (regexp (regexp-quote "VVQS invalid list input"))
           (lambda () (parse (cast '(l 2 3) Sexp))))

; ---------------------------------------------------
; Interpreter and tests

; Executes a BinopC (or Num) statement and returns a real
(define (binop-exec [b : ExprC]) : Real
  (match b
    [(NumC n) n]
    [(BinopC '+ l r) (+ (binop-exec l) (binop-exec r))]
    [(BinopC '- l r) (- (binop-exec l) (binop-exec r))]
    [(BinopC '* l r) (* (binop-exec l) (binop-exec r))]
    [(BinopC '/ l r) (/ (binop-exec l) (binop-exec r))]
    ))

; Takes in an ExprC and evaluates it
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(IdC s) (error 'interp "VVQS unbound identifier: ~a" s)]
    [(BinopC op l r) (binop-exec exp)]
    [(FundefC n a b) (interp b funs)]
    [(AppC f a)
     (define f-fn (find-function f funs))
     (match f-fn
       [(FundefC n arg body) (interp (subst arg a body) funs)]
       [_ (error 'interp "VVQS function not found: ~a" f)])]))

(check-equal? (interp (NumC 2) '()) 2)
(check-equal? (interp (parse '(+ 1 2)) '()) 3)
(check-equal? (interp (parse '(* 3 4)) '()) 12)
(check-equal? (interp (parse '(- 2 1)) '()) 1)
(check-equal? (interp (parse '(/ 3 3)) '()) 1)
(check-equal? (interp (FundefC 'a 'b  (parse '(/ 3 3))) '()) 1)
(check-exn (regexp (regexp-quote "VVQS unbound identifier:"))
           (lambda () (interp (IdC 'a) '())))


; ---------------------------------------------------

; Parses a given function definition
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (list fun-name arg) '= body) 
     (FundefC fun-name arg (parse body))]
    [_ (error 'parse-fundef "VVQS invalid input ~e" s)]))

; Test cases for parse-fundef
(check-true (exprc-equal? (parse-fundef '{def {addone x} = {+ x 1}}) (FundefC 'addone 'x (BinopC '+ (IdC 'x) (NumC 1)))))
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse-fundef 'a)))

; ---------------------------------------------------

; Parses a list of s-expressions and returns a list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    [(list (? list? s1) rest)
     (cons (parse-fundef s1) (parse-prog (list rest)))]
    [(list (? list? s1))
     (cons (parse-fundef s1) '())]
    ['() '()]
    [_ (error 'parse-prog "VVQS invalid input ~e" s)]
    ))

; Test cases for parse-prog

(check-equal? (parse-prog '{{def {f x} = {+ x 14}}})
              (list (FundefC 'f 'x (BinopC '+ (IdC 'x) (NumC 14)))))
(check-equal? (parse-prog '{{def {f x} = {+ x 14}}
                             {def {main init} = {f 2}}})
              (list (FundefC 'f 'x (BinopC '+ (IdC 'x) (NumC 14)))
                    (FundefC 'main 'init (AppC (IdC 'f) (NumC 2)))))
(check-equal? (parse-prog '()) '())
(check-exn (regexp (regexp-quote "VVQS invalid input"))
           (lambda () (parse-prog 'a)))

; ---------------------------------------------------

; Interprets the function named main from the function definitions.
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main-fn (find-main funs))
  (interp (AppC (IdC 'main) (NumC 0)) funs))

; Finds the function named main in the list of FundefC
(define (find-main [funs : (Listof FundefC)]) : FundefC
  (cond [(empty? funs) (error 'find-main "VVQS main function not found")]
        [(eq? (FundefC-name (first funs)) 'main) (first funs)]
        [else (find-main (rest funs))]))


; Finds the function with the given IdC in the list of FundefC
(define (find-function [f : ExprC] [funs : (Listof FundefC)]) : (U FundefC #f)
  (cond [(empty? funs) #f]
        [(eq? (FundefC-name (first funs)) (IdC-s (cast f IdC))) (first funs)]
        [else (find-function f (rest funs))]))

(check-equal? (find-function (IdC 'a)
                             (parse-prog '{{def {f x} = {+ x 14}}
                                           {def {main init} = {f 2}}})) #f)
(check-exn (regexp (regexp-quote "VVQS function not found:"))
           (lambda () (interp (AppC (IdC 'a) (NumC 3))
                              (parse-prog '{{def {f x} = {+ x 14}}
                                           {def {main init} = {f 2}}}))))


; Substitutes the given argument with the given value in the body of the function.
(define (subst [arg : Sexp] [val : ExprC] [body : ExprC]) : ExprC
  (match body
    [(NumC n) body]
    [(IdC s) (if (eq? s arg) val body)]
    [(BinopC op l r) (BinopC op (subst arg val l) (subst arg val r))]
    [(AppC f a) (AppC (subst arg val f) (subst arg val a))]))

(check-equal? (interp-fns (parse-prog '{{def {f x} = {+ x 14}}
                                        {def {main init} = {f 2}}}))
              16)
(check-equal? (interp-fns (list (FundefC 'main 'init (NumC 2)))) 2)
(check-exn (regexp (regexp-quote "VVQS main function not found"))
           (lambda () (interp-fns (list (FundefC 'not-main 'init (NumC 2))))))

; ---------------------------------------------------

; Takes in a sexp (some code from our language) and evaluates it
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{{def {f x} = {+ x 14}}
                            {def {main init} = {f 2}}})
              16)


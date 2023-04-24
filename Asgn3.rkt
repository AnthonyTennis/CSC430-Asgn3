#lang typed/racket

;Authors: Anthony Teciorowski, Scott Hufschmidt

(require typed/rackunit)

(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]))
(define-type ExprC (U NumC IdC AppC PlusC SubC MultC DivC leq0))
(struct NumC ([n : Real]))
(struct IdC ([s : Symbol]))
(struct AppC ([fun : Symbol] [arg : ExprC]))
(struct PlusC ([l : ExprC] [r : ExprC]))
(struct SubC ([l : ExprC] [r : ExprC]))
(struct MultC ([l : ExprC] [r : ExprC]))
(struct DivC ([l : ExprC] [r : ExprC]))
(struct leq0 ([exp : ExprC]))
(define-type binop (U Symbol))

(define (parse [s : Sexp]) : ExprC
    (cond
      [(real? s) (NumC s)]
      [(list? s)
       (case (first s)
         [(+) (PlusC (parse (second s)) (parse (third s)))]
         [() (MultC (parse (second s)) (parse (third s)))]
         [else (error 'parse "invalid list input")])]
      [else (error 'parse "invalid input")]))

#;(define (parse-fundef [s : Sexp]) : FundefC
  )

(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (list (FundefC 'a 'x (NumC 6))))

(define (interp-fns [funs : (Listof FundefC)]) : Real
  53)

(define (interp [exp : ExprC][funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(PlusC l r) (+ (interp l funs) (interp r funs))]
    [(MultC l r) ( (interp l funs) (interp r funs))]))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


(define (binop-convert [s : Symbol]) : ExprC
  (match s
    ['+ (PlusC (NumC 5) (NumC 3))]
    ['- (SubC (NumC 5) (NumC 3))]
    ['* (MultC (NumC 5) (NumC 3))]
    ['/ (DivC (NumC 5) (NumC 3))]))
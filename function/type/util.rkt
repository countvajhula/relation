#lang racket/base

(require racket/function
         contract/social
         (except-in racket/contract/base
                    predicate/c))

(provide !!
         (contract-out
          [flip functional/c]
          [if-f (-> (unconstrained-domain-> boolean?)
                    procedure?
                    procedure?
                    procedure?)]
          [true. (unconstrained-domain-> boolean?)]
          [false. (unconstrained-domain-> boolean?)]
          [arg (function/c natural-number/c procedure?)]
          [unwrap (-> list? any)]))

(define (flip f)
  (λ (x y . args)
    (apply f y x args)))

(define (if-f pred f g)
  (λ args
    (if (apply pred args)
        (apply f args)
        (apply g args))))

(define true.
  (procedure-rename (const #t)
                    'true.))

(define false.
  (procedure-rename (const #f)
                    'false.))

(define !! negate)

(define (arg n)
  (λ args
    (list-ref args n)))

(define (unwrap vs)
  (apply values vs))

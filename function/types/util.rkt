#lang racket/base

(require racket/function
         racket/list
         contract/social
         (except-in racket/contract/base
                    predicate/c))

(provide !!
         (contract-out
          [flip functional/c]
          [true. (unconstrained-domain-> boolean?)]
          [false. (unconstrained-domain-> boolean?)]
          [arg (function/c natural-number/c procedure?)]))

(define (flip f)
  (λ (x y . args)
    (apply f y x args)))

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

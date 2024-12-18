#lang racket/base


(require (except-in racket/contract/base
                    predicate/c)
         racket/undefined
         racket/match
         (only-in racket/function
                  curry)
         (only-in data/collection
                  (apply d:apply)
                  sequence?)
         (for-syntax racket/base)
         "private/util.rkt"
         contract/social)

(provide :=
         =!
         (contract-out
          [undefined? predicate/c]
          [orf variadic-function/c]
          [any? reducer/c]
          [andf variadic-function/c]
          [all? reducer/c]
          [none? reducer/c]))

(define (undefined? v)
  (eq? v undefined))

(define (andf . args)
  (match args
    ['() #t]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (and v (apply andf vs))])]))

(define (orf . args)
  (match args
    ['() #f]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (or v (apply orf vs))])]))

(define any? (curry d:apply orf))

(define all? (curry d:apply andf))

(define none? (compose not any?))

(define-alias := define)

(define-alias =! set!)

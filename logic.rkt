#lang racket/base


(require racket/contract/base
         racket/undefined
         racket/match
         (only-in racket/function
                  curry)
         (only-in data/collection
                  (apply d:apply))
         (for-syntax racket/base)
         mischief/shorthand)

(provide :=
         =!
         (contract-out
          [undefined? predicate/c]
          [orf (-> any/c ... any/c)]
          [any? (-> sequence? any/c)]
          [andf (-> any/c ... any/c)]
          [all? (-> sequence? any/c)]
          [none? (-> sequence? any/c)]))

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

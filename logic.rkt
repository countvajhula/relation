#lang racket/base


(require racket/undefined
         racket/match
         racket/contract/base
         (only-in racket/function
                  curry)
         (only-in data/collection
                  foldl)
         (for-syntax racket/base))

(provide :=
         =!
         (contract-out
          [undefined? (-> any/c boolean?)]
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

(define any? (curry foldl
                    orf
                    #f))

(define all? (curry foldl
                    andf
                    #t))

(define none? (compose not any?))

(define-syntax-rule (:= body ...)
  (define body ...))

(define-syntax-rule (=! id expr)
  (set! id expr))

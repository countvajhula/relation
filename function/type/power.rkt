#lang racket/base

(require racket/lazy-require
         racket/contract/base
         racket/generic
         racket/match
         (only-in data/collection
                  gen:collection)
         ionic)

(require "interface.rkt"
         "base.rkt"
         "composed.rkt")

;; so the power-function type can use the `power` utility
(lazy-require [relation/composition (power)])

(provide (contract-out
          [struct power-function ((composer monoid?)
                                  (f procedure?)
                                  (n number?))]
          [make-power-function (->* (procedure? number?)
                                    (#:compose-with monoid?)
                                    power-function?)]))

(define (make-power-function g n
                             #:compose-with [composer usual-composition])
  (power-function composer g n))

(struct power-function base-composed-function (f n)
  #:transparent

  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define/generic -procedure-apply procedure-apply)
   (define (keywords self)
     (-keywords (power-function-f self)))
   (define (arity self)
     (-arity (power-function-f self)))
   (define (procedure-apply self args)
     (-procedure-apply (power (power-function-f self) (power-function-n self)) args))]

  #:methods gen:collection
  [(define (conj self elem)
     (switch (elem)
             [(eq? (power-function-f self))
              (struct-copy power-function self
                           [n (add1 (power-function-n self))])]
             [else
              (composed-function (base-composed-function-composer self)
                                 (list elem self))]))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([composer (base-composed-function-composer self)]
            [f (power-function-f self)]
            [n (power-function-n self)]
            [representation
             (list 'λ
                   (list (match composer
                           [(== usual-composition) '..]
                           [(== conjoin-composition) '&&]
                           [(== disjoin-composition) '||]
                           [_ '??])
                         '^
                         n
                         f))])
       (recur representation port)))])
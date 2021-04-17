#lang racket/base

(require racket/lazy-require
         racket/contract/base
         racket/generic)

(require "procedure.rkt"
         "application-scheme.rkt"
         "base.rkt")

;; so the power-function type can use the `power` utility
(lazy-require [relation/composition (power)])

(provide (contract-out
          [struct power-function ((applier application-scheme?)
                                  (f procedure?)
                                  (n number?))]
          [make-power-function (->* (procedure? number?)
                                    (#:apply-with application-scheme?)
                                    power-function?)]))

(define (make-power-function g n
                             #:apply-with [applier empty-curried-arguments])
  (power-function applier g n))

(struct power-function function (f n)
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
     (-procedure-apply (power (power-function-f self) (power-function-n self)) args))
   (define (update-application self applier)
     (struct-copy power-function self
                  [applier #:parent function
                           applier]))
   (define (pass-args self args chirality)
     (struct-copy power-function self
                  [applier #:parent function
                           (pass (function-applier self)
                                 args
                                 chirality)]))])

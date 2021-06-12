#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         arguments
         (only-in data/collection
                  conj)
         contract/social)

(require relation/function/types/interface
         relation/function/types/base
         relation/function/types/atomic
         relation/function/types/composed
         relation/function/types/power
         relation/function/types/application-scheme
         (except-in relation/function/types/util
                    !!))

(provide (all-from-out relation/function/types/interface
                       relation/function/types/base
                       relation/function/types/atomic
                       relation/function/types/composed
                       relation/function/types/power
                       relation/function/types/application-scheme
                       relation/function/types/util)
         (contract-out
          [function-cons (binary-constructor/c procedure? base-composed-function?)]))

(define function-cons (flip conj))

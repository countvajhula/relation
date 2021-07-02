#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         arguments
         (only-in data/collection
                  conj
                  collection?)
         contract/social)

(require relation/function/type/interface
         relation/function/type/base
         relation/function/type/atomic
         relation/function/type/composed
         relation/function/type/power
         relation/function/type/application-scheme
         (except-in relation/function/type/util
                    !!))

(provide (all-from-out relation/function/type/interface
                       relation/function/type/base
                       relation/function/type/atomic
                       relation/function/type/composed
                       relation/function/type/power
                       relation/function/type/application-scheme
                       relation/function/type/util)
         (contract-out
          [function-cons (binary-constructor/c procedure? collection?)]))

(define function-cons (flip conj))

#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/list
         arguments
         (only-in data/collection
                  conj)
         contract/social
         syntax/on)

(require relation/function/types/procedure
         relation/function/types/base
         relation/function/types/atomic
         relation/function/types/composed
         relation/function/types/power
         relation/function/types/application-scheme
         (except-in relation/function/types/util
                    !!))

(provide (all-from-out relation/function/types/procedure
                       relation/function/types/base
                       relation/function/types/atomic
                       relation/function/types/composed
                       relation/function/types/power
                       relation/function/types/application-scheme
                       relation/function/types/util)
         (contract-out
          [function-cons (binary-constructor/c procedure? composed-function?)]
          [function-flat-arguments (function/c function? arguments?)]))

(define function-cons (flip conj))

(define (function-flat-arguments f)
  (flat-arguments (function-applier f)))

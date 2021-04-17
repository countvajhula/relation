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
         relation/function/types/util
         "../private/util.rkt")

(provide (all-from-out relation/function/types/procedure
                       relation/function/types/base
                       relation/function/types/atomic
                       relation/function/types/composed
                       relation/function/types/power
                       relation/function/types/application-scheme
                       relation/function/types/util)
         (contract-out
          [make-function (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              #:rest (listof procedure?)
                              function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:apply-with application-scheme?)
                  #:rest (listof procedure?)
                  function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:apply-with application-scheme?)
                                        #:rest (listof procedure?)
                                        composed-function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:apply-with application-scheme?)
                   #:rest (listof procedure?)
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-flat-arguments (function/c function? arguments?)]))

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       . fs)
  (if (singleton? fs)
      (atomic-function applier
                       (unwrap fs))
      ;; TODO: use compose interface
      (composed-function applier
                         fs
                         composer)))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-curried-arguments]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         (reverse fs)))

(define f> make-threading-function)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments])
  (make-function #:compose-with composer
                 #:apply-with applier))

(define (function-cons proc f)
  (switch (f)
    [atomic-function?
     (composed-function (function-applier f)
                        (cons proc (list (atomic-function-f f)))
                        usual-composition)] ;
    [composed-function?
     (conj f proc)]
    [else (compose proc f)]))

(define (function-flat-arguments f)
  (flat-arguments (function-applier f)))

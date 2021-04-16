#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/match
         racket/generic
         racket/set
         racket/lazy-require
         (except-in racket/list
                    empty?
                    first
                    rest)
         arguments
         syntax/parse/define
         (prefix-in b: racket/base)
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  empty?
                  first
                  rest
                  nth
                  sequence->list
                  reverse
                  repeat
                  conj)
         mischief/shorthand
         contract/social
         relation/logic
         (only-in relation/equivalence
                  in?)
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
                               #:apply-with application-scheme?
                               #:curry-on symbol?)
                              #:rest (listof procedure?)
                              function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:apply-with application-scheme?
                   #:curry-on symbol?)
                  #:rest (listof procedure?)
                  function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:apply-with application-scheme?
                                         #:curry-on symbol?)
                                        #:rest (listof procedure?)
                                        composed-function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:apply-with application-scheme?
                    #:curry-on symbol?)
                   #:rest (listof procedure?)
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?
                               #:curry-on symbol?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-flat-arguments (function/c function? arguments?)]))

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       #:curry-on [chirality 'left]
                       . fs)
  (if (singleton? fs)
      (atomic-function applier
                       chirality
                       (first fs))
      ;; TODO: use compose interface
      (composed-function applier
                         chirality
                         fs
                         composer)))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-curried-arguments]
                                 #:curry-on [chirality 'left]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         #:curry-on chirality
         (reverse fs)))

(define f> make-threading-function)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       #:curry-on [chirality 'left])
  (make-function #:compose-with composer
                 #:apply-with applier
                 #:curry-on chirality))

(define (function-cons proc f)
  (switch (f)
    [atomic-function?
     (composed-function (function-applier f)
                        (function-chirality f)
                        (cons proc (list (atomic-function-f f)))
                        usual-composition)] ;
    [composed-function?
     (conj f proc)]
    [else (compose proc f)]))

(define (function-flat-arguments f)
  (flat-arguments (function-applier f)))

#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         ionic)

(require "types.rkt"
         "composition.rkt"
         "../private/util.rkt")

(provide
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
                                function?)]
  [f> (->* ()
           (#:compose-with monoid?
            #:apply-with application-scheme?)
           #:rest (listof procedure?)
           function?)]))

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-left-curried-arguments]
                       . fs)
  (switch (fs)
          [singleton? (atomic-function applier (unwrap fs))]
          [else
           (call (apply compose-functions
                        composer
                        applier
                        _))]))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-left-curried-arguments]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         (reverse fs)))

(define f> make-threading-function)

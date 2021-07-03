#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         arguments
         ionic)

(require "type.rkt"
         "composition.rkt"
         "util.rkt"
         "../private/util.rkt")

(provide
 (contract-out
  [make-function (->* ()
                      (#:compose-with monoid?)
                      #:rest (listof procedure?)
                      function?)]
  [f (->* ()
          (#:compose-with monoid?)
          #:rest (listof procedure?)
          function?)]
  [make-threading-function (->* ()
                                (#:compose-with monoid?)
                                #:rest (listof procedure?)
                                function?)]
  [f> (->* ()
           (#:compose-with monoid?)
           #:rest (listof procedure?)
           function?)]))

;; TODO: we might want to indicate application scheme via the
;; interfaces in this file, as before
(define (make-function #:compose-with [composer usual-composition]
                       . fs)
  (curry
   (switch (fs)
           [singleton? (unwrap fs)]
           [else
            (let ([g (apply compose-functions
                            composer
                            fs)])
              g)])))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 . fs)
  (apply f
         #:compose-with composer
         (reverse fs)))

(define f> make-threading-function)

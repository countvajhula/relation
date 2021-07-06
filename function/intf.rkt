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
                      (#:thread? boolean?
                       #:compose-with monoid?)
                      #:rest (listof procedure?)
                      function?)]
  [f (->* ()
          (#:thread? boolean?
           #:compose-with monoid?)
          #:rest (listof procedure?)
          function?)]))

;; TODO: we might want to indicate application scheme via the
;; interfaces in this file, as before
(define (make-function #:thread? [thread? #f]
                       #:compose-with [composer usual-composition]
                       . fs)
  (curry
   (switch (fs)
           [singleton? (unwrap fs)]
           [else
            (let ([g (apply compose-functions
                            composer
                            ;; we reverse here for threading because
                            ;; the basic composition interfaces
                            ;; expect right-to-left ordering
                            (if thread?
                                (reverse fs)
                                fs))])
              g)])))

(define f make-function)

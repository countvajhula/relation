#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/list
         (only-in racket/function
                  curry
                  curryr)
         (only-in racket/base
                  (compose b:compose))
         arguments
         contract/social
         syntax/on)

(require "types.rkt"
         "../private/util.rkt")

(provide (contract-out
          [compose (variadic-function/c procedure? function?)]
          [conjoin (variadic-function/c procedure? function?)]
          [&& (variadic-function/c procedure? function?)]
          [disjoin (variadic-function/c procedure? function?)]
          [|| (variadic-function/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]
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
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              function?)]))

(define (compose-powers g h composer applier)
  ;; either or both could be function powers. in that case, the powers
  ;; need to be added; otherwise just incremented - actually just
  ;; map a priori to function-powers that would bbe set to 1, like a
  ;; free functor, and then compose them as function powers
  (let ([n (power-function-n g)]
        [m (power-function-n h)]
        [f (power-function-f h)])
    (make-power-function #:apply-with applier
                         #:compose-with composer
                         f
                         (+ m n))))

(define (->power-function g composer applier)
  (switch (g)
          [power-function? g]
          [else (make-power-function (~maybe-unwrap g composer) 1
                                     #:compose-with composer
                                     #:apply-with applier)]))

(define (->function g applier)
  (switch (g)
          [function? g]
          [else (atomic-function applier g)]))

(define-switch (~function-members g)
  [atomic-function? (call (.. list atomic-function-f))]
  [power-function? (call (.. list power-function-f))]
  [composed-function? (call composed-function-components)]
  [else (call list)])

(define (~maybe-unwrap g composer)
  ;; if the application is empty
  ;; unwrap atomic function
  ;; composed function if it's a singleton
  ;; and power function if the exponent is 1
  (switch (g)
          [(and function?
                (.. ~empty-application? function-applier))
           (switch (g)
                   [atomic-function? (call atomic-function-f)]
                   [(and composed-function?
                         (.. singleton?
                             composed-function-components)
                         (.. (eq? composer)
                             base-composed-function-composer))
                    (call (.. first composed-function-components))]
                   [(and power-function?
                         (.. (= 1) power-function-n)
                         (.. (eq? composer)
                             base-composed-function-composer))
                    (call power-function-f)]
                   [else g])]
          [else g]))

(define-predicate (~empty-application? applier)
  (.. (equal? empty-arguments) flat-arguments))

(define (~compatible-composition? g h composer)
  (on (g h)
      (or (all (and base-composed-function?
                    (.. (all (eq? composer))
                        (% base-composed-function-composer))))
          (and (any base-composed-function?)
               (any (not base-composed-function?))
               (.. (eq? composer)
                   base-composed-function-composer
                   (curry find base-composed-function?)
                   list))
          (all atomic-function?)
          (none function?))))

(define (~compose-as-powers g h composer applier)
  (compose-powers (->power-function g composer applier)
                  (->power-function h composer applier)
                  composer
                  applier))

(define (~compose-naively g h composer applier)
  (make-composed-function #:compose-with composer
                          #:apply-with applier
                          (~maybe-unwrap g composer)
                          (~maybe-unwrap h composer)))

(define (~compose-by-merging g h composer applier)
  (apply make-composed-function ; compose at same level
         #:apply-with applier
         #:compose-with composer
         (append (~function-members g)
                 (~function-members h))))

(define (function-compose g h composer applier)
  ;; this function assumes g and h are rich function types
  (switch (g h)
          [(or (.. (any (not ~empty-application?))
                   (% function-applier))
               (not (curryr ~compatible-composition? composer)))
           (~compose-naively g h composer applier)]
          [(.. equal? (% ~function-members))
           (~compose-as-powers g h composer applier)]
          [(any power-function?)
           (~compose-naively g h composer applier)]
          [else (~compose-by-merging g h composer applier)]))

(define (compose #:compose-with [composer usual-composition]
                 #:apply-with [applier empty-left-curried-arguments]
                 . gs)
  (switch (gs)
          [empty? (function-null #:compose-with composer
                                 #:apply-with applier)]
          [(.. empty? rest) (call first)]
          [else
           (let ([gs (reverse (map (curryr ->function applier) gs))])
             (foldl (curryr function-compose composer applier)
                    (first gs)
                    (rest gs)))]))

(define (conjoin . fs)
  (apply make-composed-function
         #:compose-with conjoin-composition
         fs))

(define (disjoin . fs)
  (apply make-composed-function
         #:compose-with disjoin-composition
         fs))

(define (negate g)
  (f not g))

(define && conjoin)
(define || disjoin)
(define !! negate)

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-left-curried-arguments]
                       . fs)
  (switch (fs)
          [singleton? (atomic-function applier (unwrap fs))]
          [else (call
                 (apply compose
                        #:compose-with composer
                        #:apply-with applier))]))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-left-curried-arguments]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         (reverse fs)))

(define f> make-threading-function)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-left-curried-arguments])
  (make-composed-function #:compose-with composer
                          #:apply-with applier))

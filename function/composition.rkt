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

(define-switch (underlying-function v)
  [power-function? (call power-function-f)]
  [atomic-function? (call atomic-function-f)]
  [else v])

(define-switch (common-underlying-function g h)
  [(with-key underlying-function eq?)
   (underlying-function g)]
  [else #f])

(define-switch (->power-function g composer applier)
  [power-function? g]
  [else (make-power-function g 1
                             #:compose-with composer
                             #:apply-with applier)])

(define (->function g applier)
  (switch (g)
          [function? g]
          [else (atomic-function applier g)]))

(define-switch (~function-members g)
  [atomic-function? (call (.. list atomic-function-f))]
  [power-function? (call (.. list power-function-f))]
  [composed-function? (call composed-function-components)]
  [else (call list)])

(define-predicate (~empty-application? applier)
  (.. (equal? empty-arguments) flat-arguments))

(define (function-compose g h composer applier)
  (switch (g h)
          [(or (with-key (.. function-applier (curryr ->function applier))
                 (any (not ~empty-application?)))
               (none function?))
           (make-composed-function #:compose-with composer
                                   #:apply-with applier
                                   g h)]
          [(any (not function?))
           (apply make-composed-function
                  #:compose-with composer
                  #:apply-with applier
                  (append (~function-members g)
                          (~function-members h)))]
          [(none base-composed-function?)
           (apply make-composed-function
                  #:compose-with composer
                  #:apply-with applier
                  (append (~function-members g)
                          (~function-members h)))]
          [(or (all (and base-composed-function?
                         (with-key base-composed-function-composer
                           (all (eq? composer)))))
               (and (any (not base-composed-function?))
                    (.. (eq? composer)
                        base-composed-function-composer
                        (curry find base-composed-function?)
                        list)))
           (switch (g h)
                   [common-underlying-function
                    (compose-powers (->power-function g composer applier)
                                    (->power-function h composer applier)
                                    composer
                                    applier)]
                   [else
                    (apply make-composed-function ; compose at same level
                           #:apply-with applier
                           #:compose-with composer
                           (append (~function-members g)
                                   (~function-members h)))])]
          [else
           ;; incompatible composition, so compose naively, but unwrap atomic
           (make-composed-function #:compose-with composer
                                   #:apply-with applier
                                   (if (atomic-function? g)
                                       (atomic-function-f g)
                                       g)
                                   (if (atomic-function? h)
                                       (atomic-function-f h)
                                       h))]))

(define (compose #:compose-with [composer usual-composition]
                 #:apply-with [applier empty-left-curried-arguments]
                 . gs)
  (switch (gs)
          [empty? (function-null #:compose-with composer
                                 #:apply-with applier)]
          [(.. empty? rest) (call first)]
          [else
           (let ([gs (reverse gs)])
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

#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/list
         (only-in racket/function
                  curry
                  curryr)
         (only-in racket/base
                  (compose b:compose))
         contract/social
         ionic)

(require "type.rkt"
         "../private/util.rkt")

(provide (contract-out
          [compose-functions (variadic-composition/c procedure? (head monoid?))]
          [compose (variadic-composition/c procedure?)]
          [conjoin (variadic-composition/c procedure?)]
          [&& (variadic-composition/c procedure?)]
          [disjoin (variadic-composition/c procedure?)]
          [|| (variadic-composition/c procedure?)]))

(define (compose-powers g h composer)
  ;; either or both could be function powers. in that case, the powers
  ;; need to be added; otherwise just incremented - actually just
  ;; map a priori to function-powers that would bbe set to 1, like a
  ;; free functor, and then compose them as function powers
  (let ([n (power-function-n g)]
        [m (power-function-n h)]
        [f (power-function-f h)])
    (make-power-function #:compose-with composer
                         f
                         (+ m n))))

(define (->power-function g composer)
  (switch (g)
    [power-function? _]
    [else (~> (~maybe-unwrap composer)
              (make-power-function 1 #:compose-with composer))]))

(define-switch (~function-members g)
  [power-function? (~> power-function-f list)]
  [composed-function? composed-function-components]
  [else list])

(define (~maybe-unwrap g composer)
  ;; if the application is empty
  ;; unwrap atomic function
  ;; composed function if it's a singleton
  ;; and power function if the exponent is 1
  (switch (g)
    [(and function?
          (or (not application-scheme?)
              empty-application?))
     (connect
      [(and composed-function?
            (~> composed-function-components
                singleton?)
            (~> base-composed-function-composer
                (eq? composer)))
       (~> composed-function-components first)]
      [(and power-function?
            (~> power-function-n (= 1))
            (~> base-composed-function-composer
                (eq? composer)))
       power-function-f]
      [else _])]
    [else _]))

(define (~compatible-composition? g h composer)
  (on (g h)
      (or (all (and base-composed-function?
                    (~> base-composed-function-composer
                        (eq? composer))))
          (and (any base-composed-function?)
               (any (not base-composed-function?))
               (~> (pass base-composed-function?)
                   base-composed-function-composer
                   (eq? composer)))
          (none function?))))

(define (~compose-as-powers g h composer)
  (compose-powers (->power-function g composer)
                  (->power-function h composer)
                  composer))

(define (~compose-naively g h composer)
  (composed-function composer
                     (list (~maybe-unwrap g composer)
                           (~maybe-unwrap h composer))))

(define (~compose-by-merging g h composer)
  ;; compose at same level
  (composed-function composer
                     ;; note that g here is expected to be the function that
                     ;; is to be applied first in the composed result, and
                     ;; that the internal representation uses left-to-right
                     ;; (list) ordering, rather than standard function
                     ;; composition (right-to-left) ordering.
                     (append (~function-members g)
                             (~function-members h))))

(define (function-compose g h composer)
  (switch (g h)
    [(or (and (any application-scheme?)
              (~> (pass application-scheme?)
                  (any (not empty-application?))))
         (not (~compatible-composition? composer)))
     (~compose-naively composer)]
    [(~> (>< ~function-members) equal?)
     (~compose-as-powers composer)]
    [(any power-function?)
     (~compose-naively composer)]
    [else (~compose-by-merging composer)]))

;; this composition interface composes functions using
;; the provided monoid, and isn't necessarily the usual
;; function composition, unlike `compose` below.
(define (compose-functions composer . gs)
  (switch (gs)
    [empty? (gen (function-null #:compose-with composer))]
    [(~> rest empty?) first]
    [else
     (~>> (-< first rest)
          (foldl (curryr function-compose composer)))]))

(define (compose . fs)
  (apply compose-functions
         usual-composition
         fs))

(define (conjoin . fs)
  (apply compose-functions
         conjoin-composition
         fs))

(define (disjoin . fs)
  (apply compose-functions
         disjoin-composition
         fs))

(define && conjoin)
(define || disjoin)

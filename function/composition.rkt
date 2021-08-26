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
          [power-function? g]
          [else (make-power-function (~maybe-unwrap g composer) 1
                                     #:compose-with composer)]))

(define-switch (~function-members g)
  [power-function? (call (~> power-function-f list))]
  [composed-function? (call composed-function-components)]
  [else (call list)])

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
             (call (~> composed-function-components first))]
            [(and power-function?
                  (~> power-function-n (= 1))
                  (~> base-composed-function-composer
                      (eq? composer)))
             (call power-function-f)]
            [else g])]
          [else g]))

(define (~compatible-composition? g h composer)
  (on (g h)
      (or (all (and base-composed-function?
                    (~> base-composed-function-composer
                        (eq? composer))))
          (and (any base-composed-function?)
               (any (not base-composed-function?))
               (~> (allow base-composed-function?)
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
                    (~> (allow application-scheme?)
                        (any (not empty-application?))))
               (not (~compatible-composition? composer)))
           (call (~compose-naively composer))]
          [(~> (>< ~function-members) equal?)
           (call (~compose-as-powers composer))]
          [(any power-function?)
           (call (~compose-naively composer))]
          [else (call (~compose-by-merging composer))]))

;; this composition interface composes functions using
;; the provided monoid, and isn't necessarily the usual
;; function composition, unlike `compose` below.
(define (compose-functions composer . gs)
  (switch (gs)
          [empty? (function-null #:compose-with composer)]
          [(~> rest empty?) (call first)]
          [else
           (foldl (curryr function-compose composer)
                  (first gs)
                  (rest gs))]))

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

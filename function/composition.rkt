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
         ionic)

(require "types.rkt"
         "../private/util.rkt")

(provide (contract-out
          [compose-functions (-> monoid? application-scheme? procedure? ... procedure?)]
          [compose (variadic-function/c procedure? procedure?)]
          [conjoin (variadic-function/c procedure? procedure?)]
          [&& (variadic-function/c procedure? procedure?)]
          [disjoin (variadic-function/c procedure? procedure?)]
          [|| (variadic-function/c procedure? procedure?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              composed-function?)]))

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
  [atomic-function? (call (~> atomic-function-f list))]
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
                (~> function-applier empty-application?))
           (connect
            [atomic-function? (call atomic-function-f)]
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
                    (~> (>< base-composed-function-composer)
                        (all (eq? composer)))))
          (and (any base-composed-function?)
               (any (not base-composed-function?))
               (~>> list
                    (find base-composed-function?)
                    base-composed-function-composer
                    (eq? composer)))
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
          [(or (~> (>< function-applier)
                   (any (not empty-application?)))
               (not (~compatible-composition? composer)))
           (call (~compose-naively composer applier))]
          [(~> (>< ~function-members) equal?)
           (call (~compose-as-powers composer applier))]
          [(any power-function?)
           (call (~compose-naively composer applier))]
          [else (call (~compose-by-merging composer applier))]))

;; TODO: avoid passing around the applier in these interfaces?
(define (compose-functions composer applier . gs)
  (switch (gs)
          [empty? (function-null #:compose-with composer
                                 #:apply-with applier)]
          [(~> rest empty?) (call first)]
          [else
           (let ([gs (reverse (map (curryr ->function applier) gs))])
             (foldl (curryr function-compose composer applier)
                    (first gs)
                    (rest gs)))]))

(define (compose . fs)
  (apply compose-functions
         usual-composition
         empty-left-curried-arguments ; maybe use that of leading function
         fs))

(define (conjoin . fs)
  (apply compose-functions
         conjoin-composition
         empty-left-curried-arguments ; maybe use that of leading function
         fs))

(define (disjoin . fs)
  (apply compose-functions
         disjoin-composition
         empty-left-curried-arguments ; maybe use that of leading function
         fs))

(define && conjoin)
(define || disjoin)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-left-curried-arguments])
  (make-composed-function #:compose-with composer
                          #:apply-with applier))

#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/match
         racket/hash
         racket/format
         racket/set
         arguments
         (prefix-in b: racket/base)
         contract/social
         (except-in data/maybe maybe/c)
         typed-stack
         relation/logic
         (only-in relation/equivalence
                  in?)
         ionic)

(require "../procedure.rkt"
         "../base.rkt"
         "../util.rkt"
         "../../../private/util.rkt")

(provide gen:application-scheme
         application-scheme/c
         recoverable-apply-error
         recoverable-apply-error?
         (contract-out
          [empty-application? (predicate/c)]
          [application-scheme? (predicate/c)]
          [pass (-> application-scheme?
                    arguments?
                    application-scheme?)]
          [flat-arguments (function/c application-scheme?
                                      arguments?)]))

;; TODO: ideally add tests for method implementations in each
;; application-scheme in a test submodule
(define-generics application-scheme
  ;; pass accepts an arguments structure representing args provided in a
  ;; single invocation, and returns an updated application-scheme instance
  ;; if the arguments are acceptable, otherwise, it raises an error that
  ;; may be either recoverable or non-recoverable. Recoverable errors
  ;; (for instance, insufficient arguments; on the other hand, excess
  ;; or invalid arguments are non-recoverable) could be handled in an
  ;; outer application scheme while non-recoverable (any other) errors
  ;; would simply be raised to the caller
  (pass application-scheme args)

  ;; flat-arguments compiles all previously supplied arguments
  ;; into a "flat" arguments structure that represents the
  ;; arguments for the invocation of the underlying function
  (flat-arguments application-scheme)

  ;; TODO: can we eliminate pass?
  ;; right-chiral (partial) arguments?

  #:defaults
  ([arguments? (define (pass this args)
                 (arguments-merge this args))
               (define (flat-arguments this)
                 this)]))

(define-predicate (empty-application? applier)
  (~> flat-arguments (equal? empty-arguments)))

(struct recoverable-apply-error exn:fail:contract ())

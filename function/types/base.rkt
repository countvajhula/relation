#lang racket/base

(require racket/contract/base
         racket/list
         racket/set
         (only-in racket/function curry)
         arguments
         syntax/on)

(require relation/logic
         (only-in relation/equivalence
                  in?)
         "application-scheme.rkt"
         "procedure.rkt"
         "util.rkt")

(provide (contract-out
          [struct function ((applier application-scheme?))]))

(struct function (applier)
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))]
          [updated-f (pass-args self
                                args
                                (chirality (function-applier self)))])
     (eval-if-saturated updated-f))))

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (call (.. (apply min) (map ~min-arity-value)))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (~min-arity f)
  (~min-arity-value (arity f)))

(define (eval-if-saturated f)
  ;; attempt to eval the function. If it fails, return a new
  ;; function with a modified applier
  (let* ([applier (function-applier f)]
         [args (flat-arguments applier)]
         [pos-args (arguments-positional args)]
         [kw-args (arguments-keyword args)])
    (with-handlers ([recoverable-apply-error?
                     ;; if it gets to the eval stage, the application scheme
                     ;; at this level has already signed off on it, but a nested
                     ;; application scheme is not yet fulfilled. We consult
                     ;; the application scheme on what to do here
                     (λ01 (exn)
                          [(curry scheme-can-continue? applier) f]
                          [else (call raise)])]
                    [exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity f))
                           (raise exn)
                           (if (scheme-can-continue? applier exn)
                               f
                               (raise exn))))]
                    [exn:fail:contract?
                     ;; presence of a keyword argument results in a premature
                     ;; contract failure that's not the arity error, even though
                     ;; that's probably what it should be since providing additional
                     ;; positional arguments results in expected behavior
                     ;; additionally, also handle invalid keyword arg here
                     (λ (exn)
                       (let-values ([(req-kw opt-kw)
                                     (keywords f)])
                         (if (or (hash-empty? kw-args)
                                 ;; the arity error is masked in the presence of keyword
                                 ;; args so we check for it again here
                                 (> (length pos-args)
                                    (~min-arity f))
                                 ;; any unexpected keywords?
                                 (any?
                                  (map (!! (in? (append req-kw opt-kw)))
                                       (hash-keys kw-args)))
                                 ;; all required arguments received?
                                 (and (subset? req-kw (hash-keys kw-args))
                                      (>= (length pos-args)
                                          (~min-arity f))))
                             (raise exn)
                             (if (scheme-can-continue? applier exn)
                                 f
                                 (raise exn)))))])
      (procedure-apply f args))))

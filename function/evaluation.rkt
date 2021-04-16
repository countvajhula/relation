#lang racket/base

(require racket/set
         arguments
         syntax/on
         relation/logic
         (only-in relation/equivalence
                  in?)
         relation/function/types/procedure
         relation/function/types/util
         relation/function/types/application-scheme)

(provide eval-if-saturated)

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (apply min (map ~min-arity-value))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (~min-arity f)
  (~min-arity-value (arity f)))

(define (eval-if-saturated f applier)
  ;; attempt to eval the function. If it fails, return a new
  ;; function with a modified applier
  (let* ([args (flat-arguments applier)]
         [pos-args (arguments-positional args)]
         [kw-args (arguments-keyword args)])
    (with-handlers ([recoverable-apply-error?
                     ;; if it gets to the eval stage, the application scheme
                     ;; at this level has already signed off on it, but a nested
                     ;; application scheme is not yet fulfilled. We consult
                     ;; the application scheme on what to do here
                     (λ (exn)
                       (update-application f (handle-failure applier exn)))]
                    [exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity f))
                           (raise exn)
                           (update-application f (handle-failure applier exn))))]
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
                             (update-application f (handle-failure applier exn)))))])
      (procedure-apply f args))))

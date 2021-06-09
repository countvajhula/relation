#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/match
         racket/hash
         racket/format
         arguments
         (prefix-in b: racket/base)
         contract/social
         (except-in data/maybe maybe/c)
         typed-stack
         ionic)

(require "../../private/util.rkt")

(provide gen:application-scheme
         application-scheme/c
         recoverable-apply-error
         recoverable-apply-error?
         (contract-out
          [empty-application? (predicate/c)]
          [application-scheme? (predicate/c)]
          [struct base-application-scheme ((chirality symbol?))]
          [struct curried-arguments
            ((chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [struct template-arguments
            ((chirality symbol?)
             (pos list?)
             (kw hash?))]
          [empty-left-curried-arguments curried-arguments?]
          [empty-right-curried-arguments curried-arguments?]
          [pass (-> application-scheme?
                    arguments?
                    symbol?
                    application-scheme?)]
          [flat-arguments (function/c application-scheme?
                                      arguments?)]
          [scheme-can-continue? (binary-predicate/c application-scheme?
                                                    exn?)]
          [chirality (function/c application-scheme?
                                 symbol?)]))

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
  (pass application-scheme args chirality)

  ;; flat-arguments compiles all previously supplied arguments
  ;; into a "flat" arguments structure that represents the
  ;; arguments for the invocation of the underlying function
  (flat-arguments application-scheme)

  ;; scheme-can-continue? is a predicate that should determine,
  ;; in light of an exception that occurred during an attempted
  ;; application, whether the scheme could continue accepting
  ;; arguments or not. For instance, curried application may
  ;; opt to continue when insufficient arguments were provided
  ;; for the application to succeed.
  (scheme-can-continue? application-scheme exception)

  ;; the direction in which fresh arguments will be parsed
  (chirality application-scheme)

  #:defaults
  ([arguments? (define (pass this args chirality)
                 (if (eq? chirality 'left)
                     (arguments-merge this args)
                     (arguments-merge args this)))
               (define (flat-arguments this)
                 this)
               (define (scheme-can-continue? this exception)
                 #f)
               (define (chirality this)
                 'left)]))

(define-predicate (empty-application? applier)
  (~> flat-arguments (equal? empty-arguments)))

(struct base-application-scheme (chirality)
  #:transparent)

(struct curried-arguments base-application-scheme (left right kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args chirality)
     ;; incorporate fresh arguments into the partial application,
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([left-args (if (eq? chirality 'left)
                          (append (curried-arguments-left this)
                                  (arguments-positional args))
                          (curried-arguments-left this))]
           [right-args (if (eq? chirality 'right)
                           ;; note order reversed for right args
                           (append (arguments-positional args)
                                   (curried-arguments-right this))
                           (curried-arguments-right this))])
       (curried-arguments chirality
                          left-args
                          right-args
                          (hash-union (curried-arguments-kw this)
                                      (arguments-keyword args)))))
   (define (flat-arguments this)
     (make-arguments (curried-arguments-positional this)
                     (curried-arguments-kw this)))
   (define (scheme-can-continue? this exception)
     #t)
   (define chirality base-application-scheme-chirality)]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([left (curried-arguments-left self)]
           [right (curried-arguments-right self)]
           [kw (curried-arguments-kw self)])
       (cond [(null? right)
              (recur (append left (list '_)) port)]
             [(null? left)
              (recur (append (list '_) right) port)]
             [else (recur (append left
                                  (list '_)
                                  right
                                  (kwhash->altlist kw))
                          port)])))])

(define empty-left-curried-arguments
  (curried-arguments 'left null null (hash)))

(define empty-right-curried-arguments
  (curried-arguments 'right null null (hash)))

(define (curried-arguments-positional args)
  (append (curried-arguments-left args)
          (curried-arguments-right args)))

(struct recoverable-apply-error exn:fail:contract ())

(struct template-arguments base-application-scheme (pos kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args chirality)
     (define n-expected-args
       (length (filter nothing? (template-arguments-pos this))))
     (define arg-stack
       (apply make-stack
              (match chirality
                ['left (arguments-positional args)]
                [_ (reverse (arguments-positional args))])))
     (define filled-in-pos-template
       (for/list ([arg (template-arguments-pos this)])
         (if (just? arg)
             arg
             (if (stack-empty? arg-stack)
                 (raise (recoverable-apply-error (~a "Not enough arguments, expected: "
                                                     n-expected-args)
                                                 (current-continuation-marks)))
                 (just (pop! arg-stack))))))
     (unless (stack-empty? arg-stack)
       ;; too many args provided
       (apply raise-arity-error
              'pass
              n-expected-args
              (append (filter-just filled-in-pos-template)
                      (stack->list arg-stack))))
     (define filled-in-kw-template
       (for/hash ([k (hash-keys (template-arguments-kw this))])
         (let ([v (hash-ref (template-arguments-kw this) k)])
           (values k (from-just (hash-ref (arguments-keyword args) k nothing)
                                v)))))
     ;; invocation kwargs should be present in blank template keys
     (define expected-keys
       (for/list ([k (hash-keys (template-arguments-kw this))]
                  #:when (nothing? (hash-ref (template-arguments-kw this) k)))
         k))
     (hash-for-each
      (arguments-keyword args)
      (λ (k v)
        (unless (member k expected-keys)
          (raise-arguments-error
           'pass
           "Unexpected keyword argument provided to template!"
           "keyword" k))))
     ;; all blank template keys should be present in invocation kwargs
     (hash-for-each
      filled-in-kw-template
      (λ (k v)
        (when (nothing? v)
          (raise (recoverable-apply-error (~a "Missing keyword argument in template!\n"
                                              "keyword: " k)
                                          (current-continuation-marks))))))
     (template-arguments chirality
                         filled-in-pos-template
                         filled-in-kw-template))

   (define (flat-arguments this)
     (make-arguments (filter-just (template-arguments-pos this))
                     (template-arguments-kw this)))

   (define (scheme-can-continue? this exception)
     #f)

   (define chirality base-application-scheme-chirality)]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([pos (template-arguments-pos self)]
           [kw (template-arguments-kw self)])
       (recur (append (map (f:curry from-just '_) pos)
                      (join-list (hash-map kw
                                           (λ (k v)
                                             (list k (from-just '_ v))))))
              port)))])

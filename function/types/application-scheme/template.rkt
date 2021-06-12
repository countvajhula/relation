#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/format
         arguments
         (prefix-in b: racket/base)
         (except-in data/maybe maybe/c)
         typed-stack)

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../../../private/util.rkt")

(provide (contract-out
          [struct template-arguments
            ((f procedure?)
             (pos list?)
             (kw hash?))]))

(define (~populate-positional-template pos args)
  (define n-expected-args
    (length (filter nothing? pos)))
  (define arg-stack
    (apply make-stack args))
  (define filled-in-pos-template
    (for/list ([arg pos])
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
  filled-in-pos-template)

(define (~populate-keyword-template kw kw-args)
  (define filled-in-kw-template
    (for/hash ([k (hash-keys kw)])
      (let ([v (hash-ref kw k)])
        (values k (from-just (hash-ref kw-args k nothing)
                             v)))))
  ;; invocation kwargs should be present in blank template keys
  (define expected-keys
    (for/list ([k (hash-keys kw)]
               #:when (nothing? (hash-ref kw k)))
      k))
  (hash-for-each
   kw-args
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
  filled-in-kw-template)

(struct template-arguments function (f pos kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this invocation-args)
     (let ([pos (template-arguments-pos this)]
           [kw (template-arguments-kw this)]
           [pos-invocation (arguments-positional invocation-args)]
           [kw-invocation (arguments-keyword invocation-args)])
       (define filled-in-pos-template
         (~populate-positional-template pos pos-invocation))
       (define filled-in-kw-template
         (~populate-keyword-template kw kw-invocation))
       (template-arguments (template-arguments-f this)
                           filled-in-pos-template
                           filled-in-kw-template)))

   (define (flat-arguments this)
     (make-arguments (filter-just (template-arguments-pos this))
                     (template-arguments-kw this)))]

  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -arity arity)
   (define/generic -keywords keywords)
   (define (procedure-apply this invocation-args)
     (let* ([f (template-arguments-f this)]
            [updated-application (pass this invocation-args)]
            [args (flat-arguments updated-application)])
       (-procedure-apply f args)))
   (define (arity this)
     (-arity (template-arguments-f this)))
   (define (keywords this)
     (-keywords (template-arguments-f this)))]

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

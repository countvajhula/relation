#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/hash
         racket/set
         arguments
         relation/logic
         (prefix-in b: racket/base)
         (only-in relation/equivalence
                  in?)
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable)
         ionic)

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../util.rkt"
         "../../../private/util.rkt"
         "private/util.rkt")

(provide (contract-out
          [struct curried-function
            ((f procedure?)
             (chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [make-curried-function (-> b:procedure?
                                     arguments?
                                     symbol?
                                     curried-function?)]))

;; TODO: check function arity in pass and reject if incompatible
(struct curried-function function (f chirality left right kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args)
     ;; incorporate fresh arguments into the partial application,
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([f (curried-function-f this)]
           [chirality (curried-function-chirality this)])
       (let ([left-args (if (eq? chirality 'left)
                            (append (curried-function-left this)
                                    (arguments-positional args))
                            (curried-function-left this))]
             [right-args (if (eq? chirality 'right)
                             ;; note order reversed for right args
                             (append (arguments-positional args)
                                     (curried-function-right this))
                             (curried-function-right this))])
         (curried-function f
                           chirality
                           left-args
                           right-args
                           (hash-union (curried-function-kw this)
                                       (arguments-keyword args))))))
   (define (flat-arguments this)
     (make-arguments (curried-function-positional this)
                     (curried-function-kw this)))]

  ;; do we want to fail at the pass level? or if we bypass pass
  ;; altogether, it would simply flat-arguments and see what happens
  ;; the latter is simpler, but the former separates concerns
  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -arity arity)
   (define/generic -keywords keywords)
   (define (procedure-apply this invocation-args)
     ;; attempt to eval the function. If it fails, return a new
     ;; function with a modified applier
     (let* ([f (curried-function-f this)]
            [updated-application (pass this invocation-args)]
            [min-arity (min-arity f)] ; check against the underlying function, to keep it simple
            [args (flat-arguments updated-application)]
            [pos-args (arguments-positional args)]
            [kw-args (arguments-keyword args)])
       (with-handlers ([recoverable-apply-error?
                        ;; if it gets to the eval stage, the application scheme
                        ;; at this level has already signed off on it, but a nested
                        ;; application scheme is not yet fulfilled. The application
                        ;; scheme determines what happens here
                        (λ (exn) updated-application)]
                       [exn:fail:contract:arity?
                        (λ (exn)
                          (if (> (length pos-args)
                                 min-arity)
                              (raise exn)
                              updated-application))]
                       [exn:fail:contract?
                        ;; presence of a keyword argument results in a premature
                        ;; contract failure that's not the arity error, even though
                        ;; that's probably what it should be since providing additional
                        ;; positional arguments results in expected behavior
                        ;; additionally, also handle invalid keyword arg here
                        (λ (exn)
                          (let-values ([(req-kw opt-kw)
                                        (-keywords f)]) ; check against underlying function
                            (if (or (hash-empty? kw-args)
                                    ;; the arity error is masked in the presence of keyword
                                    ;; args so we check for it again here
                                    (> (length pos-args)
                                       min-arity)
                                    ;; any unexpected keywords?
                                    (any?
                                     (map (!! (in? (append req-kw opt-kw)))
                                          (hash-keys kw-args)))
                                    ;; all required arguments received?
                                    (and (subset? req-kw (hash-keys kw-args))
                                         (>= (length pos-args)
                                             min-arity)))
                                (raise exn)
                                updated-application)))])
         (-procedure-apply f args))))
   (define (arity this)
     (let ([naive-arity (-arity (curried-function-f this))]
           [pos (curried-function-positional this)])
       (revise-arity naive-arity (length pos))))
   (define (keywords this)
     (let-values ([(naive-required-keywords naive-accepted-keywords)
                   (-keywords (curried-function-f this))])
       (let ([supplied-kws (hash-keys (curried-function-kw this))])
         (values (list-subtract naive-required-keywords
                                supplied-kws)
                 (and (list? naive-accepted-keywords)
                      (list-subtract naive-accepted-keywords
                                     supplied-kws))))))]

  #:methods gen:collection
  [(define/generic -conj conj)
   (define (conj self elem)
     (-conj (curried-function-f self) elem))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (curried-function-f self)))
   (define (first self)
     (-first (curried-function-f self)))
   (define (rest self)
     (struct-copy curried-function self
                  [f (-rest (curried-function-f self))]))
   (define (reverse self)
     (struct-copy curried-function self
                  [f (-reverse (curried-function-f self))]))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (curried-function-f self)))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([left (curried-function-left self)]
           [right (curried-function-right self)]
           [kw (curried-function-kw self)])
       (cond [(null? right)
              (recur (append left (list '_) (kwhash->altlist kw)) port)]
             [(null? left)
              (recur (append (list '_) right (kwhash->altlist kw)) port)]
             [else (recur (append left
                                  (list '_)
                                  right
                                  (kwhash->altlist kw))
                          port)])))])

(define (curried-function-positional args)
  (append (curried-function-left args)
          (curried-function-right args)))

(define (make-curried-function f args chirality)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)])
    (if (eq? 'left chirality)
        (curried-function f 'left pos null kw)
        (curried-function f 'right null pos kw))))

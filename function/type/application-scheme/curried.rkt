#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         contract/social
         racket/generic
         racket/hash
         racket/set
         (except-in racket/list
                    first
                    rest
                    empty?)
         arguments
         relation/logic
         (prefix-in b: racket/base)
         (only-in relation/equivalence
                  in?)
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  collection?
                  sequence?
                  conj
                  first
                  rest
                  empty?)
         ionic)

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../composed.rkt"
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
          [make-curried-function (function/c b:procedure?
                                             arguments?
                                             symbol?
                                             curried-function?)]
          [empty-curried-function curried-function?]))

(struct curried-function function (f chirality left right kw)
  #:transparent

  ;; do we want to fail at the pass level? or if we bypass pass
  ;; altogether, it would simply flat-arguments and see what happens
  ;; the latter is simpler, but the former separates concerns
  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -arity arity)
   (define/generic -keywords keywords)
   (define/generic -render-function render-function)
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
                                     supplied-kws))))))
   (define (render-function this)
     (let* ([f (curried-function-f this)]
            [left (curried-function-left this)]
            [right (curried-function-right this)]
            [kw (curried-function-kw this)]
            [inner-representation (-render-function f)]
            [marker-position (and (list? inner-representation)
                                  (index-of inner-representation 'λ))]
            [args (cond [(null? right)
                         (append left (list '···) (kwhash->altlist kw))]
                        [(null? left)
                         (append (list '···) right (kwhash->altlist kw))]
                        [else (append left
                                      (list '···)
                                      right
                                      (kwhash->altlist kw))])])
       (if (or (application-scheme? f)
               (not (list? inner-representation))
               (not marker-position)) ; unfamiliar function representation
           `(λ ,args ,inner-representation)
           (let-values ([(before after)
                         (split-at inner-representation
                                   (add1 marker-position))])
             `(,@before ,args ,@after)))))]

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
                     (curried-function-kw this)))
   (define (unwrap-application this)
     (curried-function-f this))]

  #:methods gen:collection
  [(define/generic -conj conj)
   (define (conj self elem)
     (let ([f (curried-function-f self)])
       (if (collection? f)
           (struct-copy curried-function self
                        [f (-conj f elem)])
           (struct-copy curried-function self
                        [f (-conj (-conj (function-null) f) elem)]))))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (let ([f (curried-function-f self)])
       (and (sequence? f)
            (-empty? f))))
   (define (first self)
     (let ([f (curried-function-f self)])
       (if (sequence? f)
           (struct-copy curried-function self
                        [f (-first f)])
           self)))
   (define (rest self)
     (let ([f (curried-function-f self)])
       (if (sequence? f)
           (make-curried-function (-rest f)
                                  empty-arguments
                                  (curried-function-chirality self))
           empty-curried-function)))
   (define (reverse self)
     (let ([f (curried-function-f self)])
       (if (sequence? f)
           (struct-copy curried-function self
                        [f (-reverse f)])
           self)))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (if (eq? self empty-curried-function)
         0
         (let ([f (curried-function-f self)])
           (if (sequence? f)
               (-length f)
               1))))])

(define empty-curried-function
  (curried-function (function-null)
                    'left
                    null
                    null
                    (hash)))

(define (curried-function-positional f)
  (append (curried-function-left f)
          (curried-function-right f)))

;; An application scheme (such as currying) could wrap any function, including
;; a rich composed type that implements sequence and collection interfaces,
;; and we'd like to preserve this rich functionality despite the presence
;; of the wrapper.
;; That is, for instance, we'd like to curry composed functions
;; and still be able to use the convenient sequence utilities on the
;; new function.
;; We have three options here:
;;   (1) don't support this.
;; Technically a curried function is a _different function_ from
;; the underlying function that's curried, so it need not provide the
;; same functionality as the wrapped function.
;; Yet, on the other hand, we'd like to see things in terms
;; of these orthogonal abstractions of "application" and "composition"
;; where modulating the former shouldn't have any effect on the latter,
;; and vice versa. So:
;;   (2) implement the rich interfaces in a curried-function type.
;; This would provide the desired functionality, but has the undesirable
;; drawback that _every_ curried function would reflect as a sequence,
;; that is, it would answer in the affirmative the predicate `sequence?`.
;; This could then cause unexpected errors when we invoke a sequence
;; interface such as `first` on it.
;;   (3) define two types for every application scheme – one that wraps
;; atomic functions and another that wraps composed functions. The former
;; implements none of the sequence-related interfaces, while the latter
;; does and passes through to the underlying function as needed.
;; This provides the desired functionality and doesn't bloat the underlying
;; function with interfaces it doesn't support, but unfortunately necessitates
;; _two_ types for every application scheme, even those defined by users by
;; by implementing gen:application-scheme.
;;   (4) treat the rich function types as a "functor" in the haskell sense
;; characterized by their providing implementations for the collection,
;; sequence, and countable interfaces. Application schemes being rich types
;; implement these interfaces, simply "passing through" the implementation
;; to the wrapped function if the wrapped function is a rich type as well.
;; If it isn't, then treat the scheme as a singleton, analogous to a list
;; of size 1.
;; This provides the desired functionality and doesn't necessitate the
;; atomic/composed dichotomy in (3), and though it lifts any function to
;; sequence/collection types making them more heavyweight than necessary,
;; it doesn't do so accidentally like (2) where the wrapped functions
;; could be falsely advertised as sequences, but rather as part of lifting
;; these functions to the rich type, providing reasonable implementations
;; for the rich (e.g. sequence) interfaces in all cases.
;;
;; We implement option (4) here.
(define (make-curried-function f args chirality)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)])
    (switch (f)
      [curried-function?
       (connect [(~> curried-function-chirality
                     (eq? chirality))
                 ((esc pass) args)]
                [else (~> (gen (struct-copy curried-function
                                            f
                                            [chirality chirality]))
                          ((esc pass) args))])]
      [else (if (gen (eq? chirality 'left))
                (curried-function 'left pos null kw)
                (curried-function 'right null pos kw))])))

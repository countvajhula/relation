#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         (prefix-in f: racket/function)
         racket/match
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  empty?
                  first
                  rest
                  nth
                  sequence->list
                  reverse
                  repeat)
         (only-in data/functor
                  (map f:map))
         arguments
         contract/social
         ionic)

(require "types.rkt"
         "composition.rkt"
         "intf.rkt")

(provide call
         (contract-out
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]
          [unthunk (binary-variadic-function/c procedure? any/c procedure?)]
          [flip$ functional/c]
          [flip* functional/c]
          [lift functional/c]
          [pack (binary-variadic-function/c procedure? any/c any/c)]
          [pack-map (binary-variadic-function/c procedure? any/c list?)]
          [map-values (-> procedure? any/c ... any)]
          [filter-values (-> procedure? any/c ... any)]
          [uncurry (functional/c)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [partial (unconstrained-domain-> function?)]
          [partial/template (unconstrained-domain-> function?)]))

;; from mischief/function - reproviding it via require+provide runs aground
;; of some "name is protected" error while building docs, not sure why;
;; so including the implementation directly here for now
(define call
  (make-keyword-procedure
   (lambda (ks vs f . xs)
     (keyword-apply f ks vs xs))))

(define (negate g)
  (f not g))

(define !! negate)

(define (unthunk f . args)
  (f:thunk*
   (apply f args)))

(define (flip$ f)
  (λ (x . args)
    (apply f (append args (list x)))))

(define (flip* f)
  (λ args
    (apply f (reverse args))))

(define (lift f)
  (curry f:map f))

(define (pack f . args)
  (f args))

(define (pack-map f . args)
  (map f args))

(define (map-values f . args)
  (apply values (map f args)))

(define (filter-values f . args)
  (apply values (filter f args)))

(define (uncurry g)
  (f (λ args
       (let loop ([rem-args (reverse args)])
         (match rem-args
           ['() (g)]
           [(list v) (g v)]
           [(list v vs ...) ((loop vs) v)])))))

;; HERE: the application scheme can essentially be dealt with as
;; a bolted-on aspect of the implementation - that is, independently
;; of composition and other aspects.
;;
;; these entry points should be the only way the applier is set
;; since they accept precisely the function on which the application
;; scheme is to be modulated
;; the core constructor interface should not deal with application
;; schemes -- well, maybe it's OK but the main thing is that we can
;; only modulate the applier at construction time. If at any other
;; time, it needs to wrap the underlying function (if the applier
;; is empty, we could in principle replace it, but this might
;; require an additional generic method, which may be too much
;; baggage on the procedure interface. In sum, we can assume that
;; the applier is only modulated at construction time, in all cases)
;;
;; Note that an application scheme is a fundamentally non-hierarchical
;; notion - it simply applies a function to arguments at the top level.
;; -> It's possible that AS should be treated as an independent thing
;; from function altogether, rather than as an attribute of it.
;; One benefit at the moment is being able to render arguments within
;; the function rendering at the appropriate place, which would now
;; need to render externally.
;; Well, no, maybe the right thing here is to invert the delegation,
;; so that it is not a function that has an application scheme but,
;; rather, an application scheme that has a function. This could
;; simplify the application scheme and evaluation interfaces a lot,
;; I think, for instance, we could probably eliminate scheme-can-continue?
;; and maybe even chirality can become an attribute of specific
;; schemes that need a notion of chirality
;; and we could still retain the print represenation at the applier
;; level since it is aware of the function
;;
;; In order to retain composition semantics, every "application scheme"
;; would need to be a function, too. A specialized rich type perhaps
;; (avoiding gen:application-scheme) or just a (subtype of) an
;; atomic-function that implements gen:application-scheme.
;; Again, the key, simple idea to capture is that an application scheme
;; is simply _a function that calls another function_.
;; It may be that this is simply yet another application scheme (e.g.
;; in the case of currying).
;; So maybe we define application scheme recursively as either
;;  - a ground function
;;  - a function that calls an application scheme
(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [curried-arguments?
             (connect [(~> curried-arguments-chirality
                           (eq? 'left))
                       (call (pass invocation-args))]
                      [else (pass (struct-copy curried-arguments f
                                               [chirality 'right])
                                  invocation-args)])]
            [else (call (make-curried-arguments 'left invocation-args))])))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [curried-arguments?
             (connect [(~> curried-arguments-chirality
                           (eq? 'right))
                       (call (pass invocation-args))]
                      [else (pass (struct-copy curried-arguments f
                                               [chirality 'left])
                                  invocation-args)])]
            [else (call (make-curried-arguments 'right invocation-args))])))

(define/arguments (partial args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [partial-arguments?
             (connect [(~> partial-arguments-chirality
                           (eq? 'left))
                       (call (pass invocation-args))]
                      [else (pass (struct-copy partial-arguments f
                                               [chirality 'right])
                                  invocation-args)])]
            [else (call (make-partial-arguments 'left invocation-args))])))

(define/arguments (partial/template args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)])
    (f func #:apply-with (template-arguments 'left pos kw))))

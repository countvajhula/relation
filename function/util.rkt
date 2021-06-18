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

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [curried-arguments?
             (connect [(~> curried-arguments-chirality
                           (eq? 'left))
                       (call ((esc pass) invocation-args))]
                      [else (pass (struct-copy curried-arguments f
                                               [chirality 'left])
                                  invocation-args)])]
            [else (call (make-curried-arguments invocation-args 'left))])))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [curried-arguments?
             (connect [(~> curried-arguments-chirality
                           (eq? 'right))
                       (call ((esc pass) invocation-args))]
                      [else (pass (struct-copy curried-arguments f
                                               [chirality 'right])
                                  invocation-args)])]
            [else (call (make-curried-arguments invocation-args 'right))])))

(define/arguments (partial args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (switch (f)
            [partial-arguments?
             (connect [(~> partial-arguments-chirality
                           (eq? 'left))
                       (call ((esc pass) invocation-args))]
                      [else (pass (struct-copy partial-arguments f
                                               [chirality 'right])
                                  invocation-args)])]
            [else (call (make-partial-arguments invocation-args 'left))])))

(define/arguments (partial/template args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-template-arguments f invocation-args)))

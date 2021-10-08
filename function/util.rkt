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
         qi)

(require "type.rkt"
         "composition.rkt")

(provide call
         (contract-out
          [negate functional/c]
          [!! functional/c]
          [unthunk (variadic-function/c any/c procedure? (head procedure?))]
          [flip$ functional/c]
          [flip* functional/c]
          [lift functional/c]
          [pack (variadic-function/c (head procedure?))]
          [pack-map (variadic-function/c any/c list? (head procedure?))]
          [map-values (variadic-function/c any/c any (head procedure?))]
          [filter-values (variadic-function/c any/c any (head procedure?))]
          [uncurry functional/c]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [partial (unconstrained-domain-> function?)]
          [partialr (unconstrained-domain-> function?)]
          [partial/template (unconstrained-domain-> function?)]))

;; from mischief/function - reproviding it via require+provide runs aground
;; of some "name is protected" error while building docs, not sure why;
;; so including the implementation directly here for now
(define call
  (make-keyword-procedure
   (lambda (ks vs f . xs)
     (keyword-apply f ks vs xs))))

(define (negate g)
  (compose not g))

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
  (λ args
    (let loop ([rem-args (reverse args)])
      (match rem-args
        ['() (g)]
        [(list v) (g v)]
        [(list v vs ...) ((loop vs) v)]))))

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-curried-function f invocation-args 'left)))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-curried-function f invocation-args 'right)))

(define/arguments (partial args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-partial-function f invocation-args 'left)))

(define/arguments (partialr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-partial-function f invocation-args 'right)))

(define/arguments (partial/template args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (make-template-function f invocation-args)))

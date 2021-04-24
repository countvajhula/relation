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
         contract/social)

(require "types.rkt"
         "composition.rkt")

(provide call
         (contract-out
          [unthunk (binary-variadic-function/c procedure? any/c procedure?)]
          [flip$ functional/c]
          [flip* functional/c]
          [lift functional/c]
          [pack (binary-variadic-function/c procedure? any/c any/c)]
          [pack-map (binary-variadic-function/c procedure? any/c sequence?)]
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

(define (uncurry g)
  (f (λ args
       (let loop ([rem-args (reverse args)])
         (match rem-args
           ['() (g)]
           [(list v) (g v)]
           [(list v vs ...) ((loop vs) v)])))))

(define (~curry chirality func invocation-args)
  (if (and (function? func)
           (curried-arguments? (function-applier func)))
      ;; application scheme is compatible so just apply the
      ;; new args to the existing scheme
      (pass-args func
                 invocation-args
                 chirality)
      ;; wrap the existing function with one that will be curried
      (f func
         ;; passed-in chirality overrides the attribute
         ;; so it doesn't matter that we're using 'left' here
         ;; since it's empty
         #:apply-with (pass empty-left-curried-arguments
                            invocation-args
                            chirality))))

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (~curry 'left f invocation-args)))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (~curry 'right f invocation-args)))

(define/arguments (partial args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (f func #:apply-with invocation-args)))

(define/arguments (partial/template args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)])
    (f func #:apply-with (template-arguments 'left pos kw))))

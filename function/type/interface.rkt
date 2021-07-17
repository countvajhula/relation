#lang racket/base

(require racket/generic
         racket/function
         racket/list
         (except-in racket/contract/base
                    predicate/c)
         contract/social
         (prefix-in b: racket/base)
         arguments)

(provide gen:procedure
         procedure/c
         (contract-out
          [procedure? predicate/c]
          [keywords (-> procedure?
                        (values (listof keyword?)
                                (maybe/c (listof keyword?))))]
          [arity (function/c procedure?
                             normalized-arity?)]
          [procedure-apply (binary-function/c procedure?
                                              arguments?
                                              any)]
          [render-function (function/c procedure?
                                       (or/c list?
                                             procedure?))]))

(define-generics procedure
  (keywords procedure)
  (arity procedure)
  (procedure-apply procedure args)
  (render-function procedure)

  #:derive-property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))])
     (procedure-apply self args)))

  #:fallbacks
  [(define render-function identity)]

  #:defaults
  ([b:procedure?
    (define keywords procedure-keywords)
    (define arity procedure-arity)
    (define procedure-apply apply/arguments)
    (define render-function identity)]))

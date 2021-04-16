#lang racket/base

(require racket/generic
         racket/function
         (except-in racket/contract/base
                    predicate/c)
         contract/social
         (prefix-in b: racket/base)
         arguments
         "util.rkt"
         "application-scheme.rkt")

(provide gen:procedure
         procedure/c
         (contract-out
          [keywords (-> procedure?
                        (values (listof keyword?)
                                (maybe/c (listof keyword?))))]
          [arity (function/c procedure?
                             normalized-arity?)]
          [procedure-apply (-> procedure?
                               arguments?
                               any)]
          [update-application (binary-function/c procedure?
                                                 application-scheme?
                                                 procedure?)]))

(define-generics procedure
  (keywords procedure)
  (arity procedure)
  (procedure-apply procedure args)
  ;; (pass-args procedure args)
  ;; TODO: can this and the application scheme's handle-failure be merged?
  ;; this consideration could shed light on the interplay between the two
  (update-application procedure applier)
  #:defaults
  ([b:procedure?
    (define keywords procedure-keywords)
    (define arity procedure-arity)
    ;; (define pass-args (arg 0))
    (define procedure-apply apply/arguments)
    (define update-application (arg 0))]))

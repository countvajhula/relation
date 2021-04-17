#lang racket/base

(require racket/list
         racket/contract/base
         arguments)

(require "application-scheme.rkt"
         "../evaluation.rkt")

(provide (contract-out
          [struct function ((applier application-scheme?)
                            (chirality symbol?))]))

(struct function (applier
                  chirality)
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))]
          [applier (function-applier self)]
          [updated-applier (pass applier
                                 args
                                 (function-chirality self))])
     (eval-if-saturated self updated-applier))))

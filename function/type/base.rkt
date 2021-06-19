#lang racket/base

(require racket/contract/base
         racket/list
         racket/set
         (only-in racket/function curry)
         arguments
         ionic)

(require relation/logic
         (only-in relation/equivalence
                  in?)
         "interface.rkt"
         "util.rkt")

(provide (contract-out
          [struct function ()]))

(struct function ()
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))])
     (procedure-apply self args))))

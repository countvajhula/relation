#lang racket/base

(require racket/contract/base
         racket/generic
         arguments)

(require "procedure.rkt"
         "application-scheme.rkt"
         "base.rkt")

(provide (contract-out
          [struct atomic-function ((applier application-scheme?)
                                   (chirality symbol?)
                                   (f procedure?))]))

(struct atomic-function function (f)
  #:transparent

  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define (keywords self)
     (-keywords (atomic-function-f self)))
   (define (arity self)
     (-arity (atomic-function-f self)))
   (define (procedure-apply self args)
     (apply/arguments (atomic-function-f self) args))
   (define (update-application self applier)
     (struct-copy atomic-function self
                  [applier #:parent function
                           applier]))
   (define (pass-args self args chirality)
     (struct-copy atomic-function self
                  [applier #:parent function
                           (pass (function-applier self)
                                 args
                                 chirality)]
                  [chirality #:parent function
                             chirality]))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([applier (function-applier self)]
            [f (atomic-function-f self)]
            [representation
             (list 'λ
                   applier
                   f)])
       (recur representation port)))])

(define (make-atomic-function g
                              #:apply-with [applier empty-curried-arguments]
                              #:curry-on [chirality 'left])
  (atomic-function applier
                   chirality
                   g))

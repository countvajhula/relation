#lang racket/base

(require racket/contract/base
         racket/generic
         arguments)

(require "interface.rkt"
         "base.rkt")

(provide (contract-out
          [struct atomic-function ((f procedure?))]
          [make-atomic-function (-> procedure? atomic-function?)]))

;; don't need atomic anymore since built-in type should implement
;; the necessary interfaces
(struct atomic-function function (f)
  #:transparent

  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -keywords keywords)
   (define/generic -arity arity)
   (define (keywords self)
     (-keywords (atomic-function-f self)))
   (define (arity self)
     (-arity (atomic-function-f self)))
   (define (procedure-apply self args)
     (-procedure-apply (atomic-function-f self) args))
   (define (render-function self)
     (list 'λ (atomic-function-f self)))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([representation (render-function self)])
       (recur representation port)))])

;; don't need this anymore
(define (make-atomic-function g)
  (atomic-function g))

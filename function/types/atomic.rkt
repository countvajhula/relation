#lang racket/base

(require racket/contract/base
         racket/generic
         arguments)

(require "procedure.rkt"
         "base.rkt")

(provide (contract-out
          [struct atomic-function ((f procedure?))]
          [make-atomic-function (-> procedure? atomic-function?)]))

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
     (-procedure-apply (atomic-function-f self) args))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([f (atomic-function-f self)]
            [representation
             (list 'λ
                   f)])
       (recur representation port)))])

;; don't need this anymore
(define (make-atomic-function g)
  (atomic-function g))

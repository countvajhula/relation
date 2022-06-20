#lang racket/base

(require racket/contract/base
         racket/list
         racket/set
         (only-in racket/function curry)
         arguments
         qi)

(require relation/logic
         (only-in relation/equivalence
                  in?)
         "interface.rkt"
         "util.rkt")

(provide (contract-out
          [struct function ()]))

;; formerly this was needed mainly to provide application semantics in
;; rich function types, which is now provided by the gen:procedure
;; interface itself via #:derive-property. At the moment it is useful to
;; retain the common print interface here, but if generic interfaces
;; allowed a similar way to "derive" other interfaces as it does
;; properties, we could conceivably eliminate this base type altogether
(struct function ()
  #:transparent

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([representation (render-function self)])
       (recur representation port)))])

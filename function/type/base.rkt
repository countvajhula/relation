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

;; can probably eliminate this entirely instead of retaining
;; it as an abstract base type - formerly it was needed mainly
;; to provide application semantics, which are now provided
;; by the gen:procedure interface itself via #:derive-property
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

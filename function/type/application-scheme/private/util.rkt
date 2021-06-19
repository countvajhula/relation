#lang racket/base

(require ionic)

(require "../../interface.rkt")

(provide min-arity
         revise-arity)

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (call (~>> (map ~min-arity-value) (apply min)))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (min-arity f)
  (~min-arity-value (arity f)))

(define (revise-arity v n)
  (let loop ([v v])
    (switch (v)
            ;; in the case of a curried case-lambdas, it may be
            ;; that we've received m arguments and the case arities
            ;; are a and b where a < m < b. In this case we know
            ;; we are in case b, so arguably we should simply
            ;; exclude this case from the list, flatting the list
            ;; in case of a single arity value. But for now,
            ;; we simply ensure it doesn't go below 0 since that's
            ;; an invalid arity value, since we don't immediately
            ;; know if the value is being used inside a list
            [number? (call (~> (- n) (max 0)))]
            [arity-at-least? (call (~> arity-at-least-value
                                       (- n)
                                       (max 0)
                                       arity-at-least))]
            [list? (call (~>> (map loop)))]
            [else (call (raise-argument-error 'revise-arity
                                              "normalized-arity?"
                                              _))])))

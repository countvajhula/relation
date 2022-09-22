#lang racket/base

(require qi
         (only-in racket/list empty?)
         racket/match)

(require "../../interface.rkt"
         (only-in "../../../../private/util.rkt"
                  singleton?))

(provide min-arity
         revise-arity
         list-subtract)

(define-switch ~min-arity-value
  [number? _]
  [arity-at-least? arity-at-least-value]
  [list? (~>> (map ~min-arity-value) (apply min))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              _)])

(define (min-arity f)
  (~min-arity-value (arity f)))

(define (revise-arity v n)
  (let loop ([v v])
    (switch (v)
      [number? (- n)]
      [arity-at-least? (~> arity-at-least-value
                           (- n)
                           (max 0)
                           arity-at-least)]
      ;; in the case of a curried case-lambda, it may be
      ;; that we've received m arguments and the case arities
      ;; are a and b where a < m < b. In this case we know
      ;; we are in case b, so we simply exclude the other
      ;; case from the list, flatting the list in case we
      ;; are left with a single arity value.
      [list? (~>> (map loop)
                  (filter
                   (☯ (not (and integer? negative?))))
                  (if singleton? car _))]
      [else (raise-argument-error 'revise-arity
                                  "normalized-arity?"
                                  _)])))

(define (list-subtract lst1 lst2)
  (cond [(empty? lst1) null]
        [(empty? lst2) lst1]
        [else
         (match lst1
           [(cons v vs)
            (if (member v lst2)
                (list-subtract vs lst2)
                (cons v (list-subtract vs lst2)))])]))

#lang racket/base

(require ionic
         (rename-in racket/function
                    [negate !!]
                    [conjoin &&]))

(require "../../interface.rkt"
         (only-in "../../../../private/util.rkt"
                  singleton?))

(provide min-arity
         revise-arity)

(module+ test
  (require rackunit))

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
            [number? (call (- n))]
            [arity-at-least? (call (~> arity-at-least-value
                                       (- n)
                                       (max 0)
                                       arity-at-least))]
            ;; in the case of a curried case-lambda, it may be
            ;; that we've received m arguments and the case arities
            ;; are a and b where a < m < b. In this case we know
            ;; we are in case b, so we simply exclude the other
            ;; case from the list, flatting the list in case we
            ;; are left with a single arity value.
            [list? (call (~>> (map loop)
                              (filter
                               (!! (&& integer?
                                       negative?)))
                              (if singleton? car _)))]
            [else (call (raise-argument-error 'revise-arity
                                              "normalized-arity?"
                                              _))])))

(module+ test
  (test-suite
      "revise-arity"
    (check-equal? (revise-arity 5 3) 2)
    (check-equal? (revise-arity 2 3) -1 "an invalid arity, but not the responsibility of this function to check")
    (check-equal? (revise-arity (arity-at-least 3) 2) (arity-at-least 1))
    (check-equal? (revise-arity (arity-at-least 3) 3) (arity-at-least 0))
    (check-equal? (revise-arity (arity-at-least 3) 4) (arity-at-least 0))
    (check-equal? (revise-arity (list 3 4) 2) (list 1 2))
    (check-equal? (revise-arity (list (arity-at-least 3) 4) 2) (list (arity-at-least 1) 2))
    (check-equal? (revise-arity (list (arity-at-least 3) 4) 4) (list (arity-at-least 0) 0))
    (check-equal? (revise-arity (list 3 5) 4) 1)))

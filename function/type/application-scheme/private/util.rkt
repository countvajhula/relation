#lang racket/base

(require ionic
         (only-in racket/list empty?)
         racket/match
         (rename-in racket/function
                    [negate !!]
                    [conjoin &&]))

(require "../../interface.rkt"
         (only-in "../../../../private/util.rkt"
                  singleton?))

(provide min-arity
         revise-arity
         list-subtract)

(module+ test
  (require rackunit
           rackunit/text-ui
           "../../../../tests/private/util.rkt"))

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

(define (list-subtract lst1 lst2)
  (cond [(empty? lst1) null]
        [(empty? lst2) lst1]
        [else
         (match lst1
           [(cons v vs)
            (if (member v lst2)
                (list-subtract vs lst2)
                (cons v (list-subtract vs lst2)))])]))

(module+ test

  (define tests
    (test-suite
     "application scheme utilities"

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
      (check-equal? (revise-arity (list 3 5) 4) 1))

     (test-suite
      "list-subtract"
      (check-equal? (list-subtract (list 1 2 3) (list 2)) (list 1 3))
      (check-equal? (list-subtract (list 1 2 2 3) (list 2)) (list 1 3) "removes all instances if found")
      (check-equal? (list-subtract (list 1 2 3) (list 5)) (list 1 2 3))
      (check-equal? (list-subtract (list 1 2 3) null) (list 1 2 3))
      (check-equal? (list-subtract null (list 1 2 3)) null)
      (check-equal? (list-subtract null null) null))))

  (just-do
   (run-tests tests)))

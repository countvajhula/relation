#lang racket/base

(require (prefix-in b: racket/base)
         racket/set
         (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/function
         racket/list
         (only-in data/collection
                  sequence->list)
         contract/social
         (only-in relation/equivalence (= r:=))
         (only-in relation/function
                  flip))

(require "private/util.rkt"
         "private/contract.rkt")

(define || disjoin)

(provide gen:orderable
         orderable/c
         (contract-out
          [orderable? (predicate/c)]
          [< (variadic-comparison-predicate/c orderable?)]
          [≤ (variadic-comparison-predicate/c orderable?)]
          [<= (variadic-comparison-predicate/c orderable?)]
          [≥ (variadic-comparison-predicate/c orderable?)]
          [>= (variadic-comparison-predicate/c orderable?)]
          [> (variadic-comparison-predicate/c orderable?)]
          (sort (->* (procedure? sequence?)
                     (#:key (maybe/c (encoder/c orderable?)))
                     sequence?))
          (min (variadic-comparison-selection/c orderable?))
          (max (variadic-comparison-selection/c orderable?))))

(define-generics orderable
  (less-than? orderable other)
  (less-than-or-equal? orderable other)
  (greater-than-or-equal? orderable other)
  (greater-than? orderable other)
  #:fallbacks [(define/generic generic-lt less-than?)
               (define/generic generic-lte less-than-or-equal?)
               (define less-than-or-equal? (|| generic-lt
                                               r:=))
               (define greater-than-or-equal? (flip generic-lte))
               (define greater-than? (flip generic-lt))]
  #:fast-defaults ([number?
                    (define less-than? b:<)
                    (define less-than-or-equal? b:<=)
                    (define greater-than-or-equal? b:>=)
                    (define greater-than? b:>)]
                   [string?
                    (define less-than? string<?)
                    (define less-than-or-equal? string<=?)
                    (define greater-than-or-equal? string>=?)
                    (define greater-than? string>?)]
                   [bytes?
                    (define less-than? bytes<?)
                    (define less-than-or-equal?
                      (|| bytes=?
                          bytes<?))
                    (define greater-than-or-equal?
                      (|| bytes=?
                          bytes>?))
                    (define greater-than? bytes>?)]
                   [char?
                    (define less-than? char<?)
                    (define less-than-or-equal? char<=?)
                    (define greater-than-or-equal? char>=?)
                    (define greater-than? char>?)]
                   [set?
                    (define less-than? proper-subset?)
                    (define less-than-or-equal? subset?)
                    (define greater-than-or-equal? (flip subset?))
                    (define greater-than? (flip proper-subset?))]))

(define (< #:key [key #f] . args)
  (if key
      (apply < (map key args))
      (check-pairwise less-than? args)))

(define (≤ #:key [key #f] . args)
  (if key
      (apply ≤ (map key args))
      (check-pairwise less-than-or-equal? args)))

(define (≥ #:key [key #f] . args)
  (if key
      (apply ≥ (map key args))
      (check-pairwise greater-than-or-equal? args)))

(define (> #:key [key #f] . args)
  (if key
      (apply > (map key args))
      (check-pairwise greater-than? args)))

(define (sort less-than? #:key [key #f] seq)
  (if (set-member? (set < >) less-than?)
      (b:sort (sequence->list seq)
              (curryr less-than? #:key key))
      (raise-argument-error 'sort
                            "Either < or >"
                            less-than?)))

(define (min #:key [key #f] . args)
  (first (sort < #:key key args)))

(define (max #:key [key #f] . args)
  (first (sort > #:key key args)))

(define <= ≤)
(define >= ≥)

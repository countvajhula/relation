#lang racket

(provide gen:comparable
         comparable?
         comparable/c
         <
         <=
         =
         >=
         >
         ≤
         ≥)

(require (prefix-in b: racket/base)
         racket/generic)

;; define a generic `comparable` interface
(define-generics comparable
  (< comparable other)
  (<= comparable other)
  (= comparable other)
  (>= comparable other)
  (> comparable other)
  #:defaults ([number?
               (define (< comparable other)
                 (b:< comparable other))
               (define (<= comparable other)
                 (b:<= comparable other))
               (define (= comparable other)
                 (b:= comparable other))
               (define (>= comparable other)
                 (b:>= comparable other))
               (define (> comparable other)
                 (b:> comparable other))]
              [string?
               (define (< comparable other)
                 (string<? comparable other))
               (define (<= comparable other)
                 (string<=? comparable other))
               (define (= comparable other)
                 (string=? comparable other))
               (define (>= comparable other)
                 (string>=? comparable other))
               (define (> comparable other)
                 (string>? comparable other))]))

(define ≤ <=)
(define ≥ >=)

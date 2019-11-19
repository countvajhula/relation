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
  (< comparable . others)
  (<= comparable . others)
  (= comparable . others)
  (>= comparable . others)
  (> comparable . others)
  #:defaults ([number?
               (define (< comparable . others)
                 (apply b:< (cons comparable others)))
               (define (<= comparable . others)
                 (apply b:<= comparable others))
               (define (= comparable . others)
                 (apply b:= comparable others))
               (define (>= comparable . others)
                 (apply b:>= comparable others))
               (define (> comparable . others)
                 (apply b:> comparable others))]
              [string?
               (define (< comparable . others)
                 (apply string<? (cons comparable others)))
               (define (<= comparable . others)
                 (apply string<=? (cons comparable others)))
               (define (= comparable . others)
                 (apply string=? (cons comparable others)))
               (define (>= comparable . others)
                 (apply string>=? (cons comparable others)))
               (define (> comparable . others)
                 (apply string>? (cons comparable others)))]))

(define ≤ <=)
(define ≥ >=)

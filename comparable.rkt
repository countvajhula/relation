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
                 (apply string>? (cons comparable others)))]
              [char?
               (define (< comparable . others)
                 (apply char<? (cons comparable others)))
               (define (<= comparable . others)
                 (apply char<=? (cons comparable others)))
               (define (= comparable . others)
                 (apply char=? (cons comparable others)))
               (define (>= comparable . others)
                 (apply char>=? (cons comparable others)))
               (define (> comparable . others)
                 (apply char>? (cons comparable others)))]
              [set?
               (define/generic generic-<= <=)
               (define/generic generic-< <)
               (define (check-sequence vals check?)
                 (let ([current (car vals)]
                       [remaining (cdr vals)])
                   (if (empty? remaining)
                       #t
                       (and (check? current (car remaining))
                            (check-sequence remaining check?)))))
               (define (< comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence vals proper-subset?)))
               (define (<= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence vals subset?)))
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence vals equal?)))
               (define (>= comparable . others)
                 (not (apply generic-< (cons comparable others))))
               (define (> comparable . others)
                 (not (apply generic-<= (cons comparable others))))]))

(define ≤ <=)
(define ≥ >=)

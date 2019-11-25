#lang racket

(require (prefix-in b: racket/base)
         racket/generic)

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
               (define/generic generic-= =)
               (define/generic generic-> >)
               (define (check-sequence check? vals)
                 (let ([current (car vals)]
                       [remaining (cdr vals)])
                   (if (empty? remaining)
                       #t
                       (and (check? current (car remaining))
                            (check-sequence check? remaining)))))
               (define (< comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence proper-subset? vals)))
               (define (<= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence subset? vals)))
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence equal? vals)))
               (define (>= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence subset?
                                   (reverse vals))))
               (define (> comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence proper-subset?
                                   (reverse vals))))]))

(define ≤ <=)
(define ≥ >=)

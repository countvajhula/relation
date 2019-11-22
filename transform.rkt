#lang racket

(require (rename-in racket/base
                    (round b:round))
         racket/set
         racket/dict)

(provide ->boolean
         ->string
         ->number
         ->list)

(define (->boolean v)
  (if v
      #t
      #f))

(define (->string v)
  (cond [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [(number? v) (number->string v)]
        [(keyword? v) (keyword->string v)]
        [(list? v) (list->string v)]
        [else (error "Unsupported type!" v)]))

(define (->number v)
  (cond [(number? v) v]
        [(string? v) (string->number v)]
        [(char? v) (char->integer v)]
        [else (error "Unsupported type!" v)]))

(define (->inexact v)
  (cond [(inexact? v) v]
        [(exact? v) (exact->inexact v)]
        [else (error "Unsupported type!" v)]))

(define (->integer v #:round [round 'down])
  (cond [(integer? v) v]
        [(number? v) (cond [(eq? round 'down) (floor v)]
                           [(eq? round 'up) (ceiling v)]
                           [(eq? round 'nearest) (b:round v)]
                           [else (error "Unrecognized rounding policy!" round)])]
        [(string? v) (->integer (->number v))]
        [else (error "Unsupported type!" v)]))

(define (->list v)
  (cond [(string? v) (string->list v)]
        [(vector? v) (vector->list v)]
        [(dict? v) (dict->list v)]
        [(set? v) (set->list v)]
        [(syntax? v) (syntax->list v)]
        [else (error "Unsupported type!" v)]))

(define (->vector v)
  (cond [(vector? v) v]
        [(list? v) (list->vector v)]
        [else (->vector (->list v))]))

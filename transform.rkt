#lang racket

(require (rename-in racket/base
                    (round b:round))
         racket/contract/base
         racket/format
         racket/set
         racket/dict
         racket/stream)

(provide ->boolean
         ->string
         ->number
         ->inexact
         ->exact
         ->integer
         ->list
         ->vector
         ->symbol
         ->keyword
         ->bytes
         ->char
         ->stream
         ->set
         ->syntax
         ->code
         ->values)

(define (->boolean v)
  (if v
      #t
      #f))

(define (->string v)
  (cond [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [(number? v) (number->string v)]
        [(keyword? v) (keyword->string v)]
        [((listof char?) v) (list->string v)]
        [(bytes? v) (bytes->string/locale v)]
        [(list? v) (~a v)]
        [else (error "Unsupported type!" v)]))

(define (->number v)
  (cond [(number? v) v]
        [(string? v) (string->number v)]
        [(char? v) (char->integer v)]
        [else (error "Unsupported type!" v)]))

(define (->inexact v)
  (cond [((and/c number? inexact?) v) v]
        [(number? v) (exact->inexact v)]
        [else (->inexact (->number v))]))

(define (->exact v)
  (cond [((and/c number? exact?) v) v]
        [(number? v) (inexact->exact v)]
        [else (->exact (->number v))]))

(define (->integer v #:round [round 'down])
  (cond [(integer? v) v]
        [(number? v) (cond [(eq? round 'down) (->exact (floor v))]
                           [(eq? round 'up) (->exact (ceiling v))]
                           [(eq? round 'nearest) (->exact (b:round v))]
                           [else (error "Unrecognized rounding policy!" round)])]
        [(char? v) char->integer]
        [(string? v) (->integer (->number v))]
        [else (error "Unsupported type!" v)]))

(define (->list v)
  (cond [(list? v) v]
        [(string? v) (string->list v)]
        [(vector? v) (vector->list v)]
        [(dict? v) (dict->list v)]
        [(set? v) (set->list v)]
        [(syntax? v) (syntax->list v)]
        [(bytes? v) (bytes->list v)]
        [(sequence? v) (sequence->list v)]
        [else (error "Unsupported type!" v)]))

(define (->vector v)
  (cond [(vector? v) v]
        [(list? v) (list->vector v)]
        [else (->vector (->list v))]))

(define (->symbol v)
  (cond [(symbol? v) v]
        [(string? v) (string->symbol v)]
        [(keyword? v) (->symbol (->string v))]
        [else (error "Unsupported type!" v)]))

(define (->keyword v)
  (cond [(keyword? v) v]
        [(string? v) (string->keyword v)]
        [(symbol? v) (->keyword (->string v))]
        [else (error "Unsupported type!" v)]))

(define (->bytes v)
  (cond [(bytes? v) v]
        [(list? v) (list->bytes v)]
        [(string? v) (->bytes (map char->integer (->list v)))]
        [else (error "Unsupported type!" v)]))

(define (->char v)
  (cond [(char? v) v]
        [(integer? v) (integer->char v)]
        [(string? v) (string-ref v 0)]
        [(symbol? v) (->char (->string v))]
        [else (error "Unsupported type!" v)]))

(define (->stream v)
  (cond [(stream? v) v]
        [(sequence? v) (sequence->stream v)]
        [else (error "Unsupported type!" v)]))

(define (->set v)
  (cond [(set? v) v]
        [(list? v) (list->set v)]
        [(sequence? v) (->set (->list v))]
        [else (error "Unsupported type!" v)]))

(define (->syntax v)
  (cond [(syntax? v) v]
        [else (datum->syntax #f v)]))

(define (->code v)
  (cond [(syntax? v) (syntax->datum v)]
        [else v]))

(define (->values v)
  (cond [(vector? v) (vector->values v)]
        [else (->values (->vector v))]))

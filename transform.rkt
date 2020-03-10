#lang racket/base

(require (rename-in racket/base
                    (round b:round))
         racket/contract/base
         racket/format
         racket/string
         racket/set
         racket/dict
         racket/vector
         racket/stream
         racket/generator
         racket/sequence)

(provide (contract-out
          [->boolean (-> any/c boolean?)]
          [->string (-> any/c string?)]
          [->number (-> any/c number?)]
          [->inexact (-> any/c inexact?)]
          [->exact (-> any/c exact?)]
          [->integer (->* (any/c)
                          (#:round (one-of/c 'up
                                             'down
                                             'nearest))
                          integer?)]
          [->list (-> any/c list?)]
          [->vector (-> any/c vector?)]
          [->symbol (-> any/c symbol?)]
          [->keyword (-> any/c keyword?)]
          [->bytes (-> any/c bytes?)]
          [->char (-> any/c char?)]
          [->stream (-> any/c stream?)]
          [->generator (-> any/c generator?)]
          [->set (-> any/c set?)]
          [->syntax (-> any/c syntax?)]
          [->code (-> any/c any/c)]
          [->values (-> any/c any)]))

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
        [(sequence? v) (->string (->list v))]
        [(generator? v) (->string (->list v))]
        [else (~a v)]))

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
                           [(eq? round 'nearest) (->exact (b:round v))])]
        [else (->integer (->number v))]))

(define (->list v)
  (cond [(list? v) v]
        [(string? v) (string->list v)]
        [(vector? v) (vector->list v)]
        [(dict? v) (dict->list v)]
        [(set? v) (set->list v)]
        [(syntax? v) (syntax->list v)]
        [(bytes? v) (bytes->list v)]
        [(sequence? v) (sequence->list v)]
        [(generator? v) (->list (->stream v))]
        [(generic-set? v) (set->list v)]
        [(struct? v) (->list (->vector v))]
        [else (error "Unsupported type!" v)]))

(define (->vector v)
  (cond [(vector? v) (if (immutable? v)
                         v
                         (vector->immutable-vector v))]
        [(list? v) (->vector (list->vector v))]
        [(struct? v) (->vector (vector-drop (struct->vector v) 1))]
        [else (->vector (->list v))]))

(define (->symbol v)
  (cond [(symbol? v) v]
        [(string? v) (string->symbol v)]
        [else (->symbol (->string v))]))

(define (->keyword v)
  (cond [(keyword? v) v]
        [(string? v) (string->keyword v)]
        [else (->keyword (->string v))]))

(define (->bytes v)
  (cond [(bytes? v) v]
        [(list? v) (list->bytes v)]
        [(string? v) (->bytes (map char->integer (->list v)))]
        [else (error "Unsupported type!" v)]))

(define (->char v)
  (cond [(char? v) v]
        [(integer? v) (integer->char v)]
        [((and/c non-empty-string? (string-len/c 2)) v) (string-ref v 0)]
        [((and/c (non-empty-listof any/c)
                 (property/c length (=/c 1))) v)
         (->char (list-ref v 0))]
        [(symbol? v) (->char (->string v))]
        [else (error "Unsupported type!" v)]))

(define (->stream v)
  (cond [(stream? v) v]
        [(sequence? v) (sequence->stream v)]
        [(generator? v) (->stream (in-producer v (void)))]
        [else (error "Unsupported type!" v)]))

(define (->generator v)
  (cond [(generator? v) v]
        [(sequence? v) (sequence->generator v)]
        [else (error "Unsupported type!" v)]))

(define (->set v)
  (cond [(set? v) v]
        [(list? v) (list->set v)]
        [else (->set (->list v))]))

(define (->syntax v)
  (cond [(syntax? v) v]
        [else (datum->syntax #f v)]))

(define (->code v)
  (cond [(syntax? v) (syntax->datum v)]
        [else v]))

(define (->values v)
  (cond [(vector? v) (vector->values v)]
        [else (->values (->vector v))]))

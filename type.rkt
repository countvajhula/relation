#lang racket/base

(require (only-in racket/base
                  (round b:round))
         (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/format
         racket/string
         racket/set
         racket/dict
         racket/vector
         racket/stream
         racket/generator
         racket/sequence
         racket/match
         syntax/parse/define
         (only-in racket/function
                  identity
                  negate
                  thunk*)
         threading
         (only-in data/collection
                  for-each
                  collection?
                  conj
                  conj*)
         contract/social
         (only-in relation/composition
                  ID
                  reify))

(provide :
         make!
         :!
         (contract-out
          [make (case->
                 (-> collection?)
                 (-> collection? any/c collection?)
                 (-> collection? #:rest list? collection?))]
          [->boolean (predicate/c)]
          [->string (encoder/c string?)]
          [->number (encoder/c number?)]
          [->inexact (encoder/c inexact?)]
          [->exact (encoder/c exact?)]
          [->integer (->* (any/c)
                          (#:round (one-of/c 'up
                                             'down
                                             'nearest))
                          integer?)]
          [->list (encoder/c list?)]
          [->vector (encoder/c vector?)]
          [->symbol (encoder/c symbol?)]
          [->keyword (encoder/c keyword?)]
          [->bytes (encoder/c bytes?)]
          [->char (encoder/c char?)]
          [->stream (encoder/c stream?)]
          [->generator (->* (any/c)
                            (any/c)
                            generator?)]
          [->set (encoder/c set?)]
          [->syntax (->* (any/c)
                         (syntax?)
                         syntax?)]
          [->symex (function/c)]
          [string->symex (decoder/c string?)]
          [->values (-> any/c any)]
          [->hash (encoder/c hash?)]
          [->procedure (encoder/c procedure?)]))

(define make
  (case-lambda
    [() ID]
    [(form element)
     (conj form element)]
    [(form . elements)
     (apply conj* form elements)]))

(define (: . args)
  (match args
    [(list) (apply make args)]
    [(list element form)
     (if (collection? form)
         (make form element)
         (cons element form))]
    [(list elements ... form)
     (if (collection? form)
         (apply make (if (vector? form)
                         (cons form elements)
                         (cons form (reverse elements))))
         args)]))

(define-simple-macro (make! form:id elements ...)
  (set! form (make form elements ...)))

(define-simple-macro (:! elements ... form:id)
  (set! form (: elements ... form)))

(define (->boolean v)
  (if v #t #f))

(define (->string v)
  (cond [(string? v) (if (immutable? v)
                         v
                         (string->immutable-string v))]
        [(symbol? v) (~> v
                         symbol->string
                         ->string)]
        [(number? v) (~> v
                         number->string
                         ->string)]
        [(keyword? v) (~> v
                          keyword->string
                          ->string)]
        [((listof char?) v) (~> v
                                list->string
                                ->string)]
        [(bytes? v) (~> v
                        bytes->string/locale
                        ->string)]
        [(list? v) (~> v
                       ~a
                       ->string)]
        [(sequence? v) (~> v
                           ->list
                           ->string)]
        [(generator? v) (~> v
                            ->list
                            ->string)]
        [else (~> v
                  ~a
                  ->string)]))

(define (->number v)
  (cond [(number? v) v]
        [(string? v) (string->number v)]
        [(char? v) (char->integer v)]
        [(eq? v ID) (reify v 0 +)]
        [else (error '->number "Unsupported type ~a!" v)]))

(define (->inexact v)
  (cond [((and/c number? inexact?) v) v]
        [(number? v) (exact->inexact v)]
        [else (~> v
                  ->number
                  ->inexact)]))

(define (->exact v)
  (cond [((and/c number? exact?) v) v]
        [(number? v) (inexact->exact v)]
        [else (~> v
                  ->number
                  ->exact)]))

(define (->integer v #:round [round 'down])
  (cond [(integer? v) v]
        [(number? v) (cond [(eq? round 'down)
                            (~> v
                                floor
                                ->exact)]
                           [(eq? round 'up)
                            (~> v
                                ceiling
                                ->exact)]
                           [(eq? round 'nearest)
                            (~> v
                                b:round
                                ->exact)])]
        [else (~> v
                  ->number
                  ->integer)]))

(define (->list v)
  (cond [(list? v) v]
        [(string? v) (string->list v)]
        [(vector? v) (vector->list v)]
        [(dict? v) (dict->list v)]
        [(set? v) (set->list v)]
        [(syntax? v) (syntax->list v)]
        [(bytes? v) (bytes->list v)]
        [(sequence? v) (sequence->list v)]
        [(generator? v) (~> v
                            ->stream
                            ->list)]
        [(generic-set? v) (set->list v)]
        [(struct? v) (~> v
                         ->vector
                         ->list)]
        [else (error '->list "Unsupported type ~a!" v)]))

(define (->vector v)
  (cond [(vector? v) (if (immutable? v)
                         v
                         (vector->immutable-vector v))]
        [(list? v) (~> v
                       list->vector
                       ->vector)]
        [(struct? v) (~> v
                         struct->vector
                         (vector-drop 1)
                         ->vector)]
        [else (~> v
                  ->list
                  ->vector)]))

(define (->symbol v)
  (cond [(symbol? v) v]
        [(string? v) (string->symbol v)]
        [else (~> v
                  ->string
                  ->symbol)]))

(define (->keyword v)
  (cond [(keyword? v) v]
        [(string? v) (string->keyword v)]
        [else (~> v
                  ->string
                  ->keyword)]))

(define (->bytes v)
  (cond [(bytes? v) v]
        [(list? v) (list->bytes v)]
        [(string? v) (~> v
                         ->list
                         (map char->integer _)
                         ->bytes)]
        [else (~> v
                  ->string
                  ->bytes)]))

(define (->char v)
  (cond [(char? v) v]
        [(integer? v) (integer->char v)]
        [((and/c non-empty-string? (string-len/c 2)) v) (string-ref v 0)]
        [((and/c (non-empty-listof any/c)
                 (property/c length (=/c 1))) v)
         (~> v
             (list-ref 0)
             ->char)]
        [(symbol? v) (~> v
                         ->string
                         ->char)]
        [else (error '->char "Unsupported type ~a!" v)]))

(define (->stream v)
  (cond [(stream? v) v]
        [(sequence? v) (sequence->stream v)]
        [(generator? v) (~> v
                            (in-producer (void))
                            ->stream)]
        [else (error '->stream "Unsupported type ~a!" v)]))

(define (sequence->generator seq [return (void)])
  (generator ()
    (for-each (λ (v)
                (yield v)
                (void))
              seq)
    return))

(define (->generator v [return (void)])
  (cond [(generator? v) v]
        [(sequence? v) (sequence->generator v return)]
        [else (error '->generator "Unsupported type ~a!" v)]))

(define (->set v)
  (cond [(set? v) v]
        [(list? v) (list->set v)]
        [else (~> v
                  ->list
                  ->set)]))

(define (->syntax v [ctx #f])
  (cond [(syntax? v) v]
        [else (datum->syntax (if ctx ctx #f) v)]))

(define (->symex v)
  (cond [(syntax? v) (syntax->datum v)]
        [else v]))

(define (string->symex v)
  (read (open-input-string v)))

(define (->values v)
  (cond [(vector? v) (vector->values v)]
        [else (~> v
                  ->vector
                  ->values)]))

(define (->hash v)
  (cond [(hash? v) v]
        [(dict? v) (make-immutable-hash v)]
        [(eq? v ID) (reify v (hash))]
        [else (error '->hash "Unsupported type ~a!" v)]))

(define (->procedure v)
  (cond [(procedure? v) v]
        [else (thunk* v)]))

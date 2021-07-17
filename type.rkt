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
         (only-in data/collection
                  for-each
                  collection?
                  conj
                  extend)
         contract/social
         (only-in relation/composition
                  ID
                  reify)
         ionic)

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
     (extend form elements)]))

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

(define-switch (->string v)
  [string? (connect [immutable? v]
                    [else (call string->immutable-string)])]
  [symbol? (call (~> symbol->string
                     ->string))]
  [number? (call (~> number->string
                     ->string))]
  [keyword? (call (~> keyword->string
                      ->string))]
  [(listof char?) (call (~> list->string
                            ->string))]
  [bytes? (call (~> bytes->string/locale
                    ->string))]
  [list? (call (~> ~a
                   ->string))]
  [sequence? (call (~> ->list
                       ->string))]
  [generator? (call (~> ->list
                        ->string))]
  [else (call (~> ~a
                  ->string))])

(define-switch (->number v)
  [number? v]
  [string? (call string->number)]
  [char? (call char->integer)]
  [(eq? ID) (call (reify 0 +))]
  [else (error '->number "Unsupported type ~a!" v)])

(define-switch (->inexact v)
  [(and number? inexact?) v]
  [number? (call exact->inexact)]
  [else (call (~> ->number
                  ->inexact))])

(define-switch (->exact v)
  [(and number? exact?) v]
  [number? (call inexact->exact)]
  [else (call (~> ->number
                  ->exact))])

(define (->integer v #:round [round 'down])
  (switch (v)
    [integer? v]
    [number? (switch (round)
               [(eq? 'down)
                  (~> (v)
                      floor
                      ->exact)]
               [(eq? 'up)
                (~> (v)
                    ceiling
                    ->exact)]
               [(eq? 'nearest)
                (~> (v)
                    b:round
                    ->exact)])]
    [else (call (~> ->number
                    ->integer))]))

(define-switch (->list v)
  [list? v]
  [string? (call string->list)]
  [vector? (call vector->list)]
  [dict? (call dict->list)]
  [set? (call set->list)]
  [syntax? (call syntax->list)]
  [bytes? (call bytes->list)]
  [sequence? (call sequence->list)]
  [generator? (call (~> ->stream
                        ->list))]
  [generic-set? (call set->list)]
  [struct? (call (~> ->vector
                     ->list))]
  [else (error '->list "Unsupported type ~a!" v)])

(define-switch (->vector v)
  [vector? (connect [immutable? v]
                    [else (call vector->immutable-vector)])]
  [list? (call (~> list->vector
                   ->vector))]
  [sequence? (call (~> ->list
                       ->vector))]
  [struct? (call (~> struct->vector
                     (vector-drop 1)
                     ->vector))]
  [else (call (~> ->list
                  ->vector))])

(define-switch (->symbol v)
  [symbol? v]
  [string? (call string->symbol)]
  [else (call (~> ->string
                  ->symbol))])

(define-switch (->keyword v)
  [keyword? v]
  [string? (call string->keyword)]
  [else (call (~> ->string
                  ->keyword))])

(define-switch (->bytes v)
  [bytes? v]
  [list? (call list->bytes)]
  [string? (call (~> ->list
                     (map char->integer _)
                     ->bytes))]
  [else (call (~> ->string
                  ->bytes))])

(define-switch (->char v)
  [char? v]
  [integer? (call integer->char)]
  [(and non-empty-string?
        (esc (string-len/c 2)))
   (call (string-ref 0))]
  [(and (esc (non-empty-listof any/c))
        (esc (property/c length (=/c 1))))
   (call (~> (list-ref 0)
             ->char))]
  [symbol? (call (~> ->string
                     ->char))]
  [else (error '->char "Unsupported type ~a!" v)])

(define-switch (->stream v)
  [stream? v]
  [sequence? (call sequence->stream)]
  [generator? (call (~> (in-producer (void))
                        ->stream))]
  [else (error '->stream "Unsupported type ~a!" v)])

(define (sequence->generator seq [return (void)])
  (generator ()
    (for-each (λ (v)
                (yield v)
                (void))
              seq)
    return))

(define (->generator v [return (void)])
  (switch (v)
    [generator? v]
    [sequence? (call (sequence->generator return))]
    [else (error '->generator "Unsupported type ~a!" v)]))

(define-switch (->set v)
  [set? v]
  [list? (call list->set)]
  [else (call (~> ->list
                  ->set))])

(define (->syntax v [ctx #f])
  (switch (v)
    [syntax? v]
    [else (datum->syntax (if ctx ctx #f) v)]))

(define-switch (->symex v)
  [syntax? (call syntax->datum)]
  [else v])

(define (string->symex v)
  (read (open-input-string v)))

(define-switch (->values v)
  [vector? (call vector->values)]
  [else (call (~> ->vector
                  ->values))])

(define-switch (->hash v)
  [hash? v]
  [dict? (call make-immutable-hash)]
  [(eq? ID) (call (reify (hash)))]
  [else (error '->hash "Unsupported type ~a!" v)])

(define-switch (->procedure v)
  [procedure? v]
  [else (thunk* v)])

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
         qi
         mischief/shorthand
         version-case
         (for-syntax racket/base))

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(provide :
         make!
         :!
         (contract-out
          [make (case->
                 (-> collection?)
                 (-> collection? any/c collection?)
                 (-> collection? #:rest list? collection?))]
          [->boolean predicate/c]
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
          [->symex function/c]
          [string->symex (decoder/c string?)]
          [->values (encoder/c any)]
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

(define-syntax-parse-rule (make! form:id elements ...)
  (set! form (make form elements ...)))

(define-syntax-parse-rule (:! elements ... form:id)
  (set! form (: elements ... form)))

(define (->boolean v)
  (if v #t #f))

(define-switch ->string
  [string? (switch
             [immutable? _]
             [else string->immutable-string])]
  [symbol? (~> symbol->string
               ->string)]
  [number? (~> number->string
               ->string)]
  [keyword? (~> keyword->string
                ->string)]
  [(listof char?) (~> list->string
                      ->string)]
  [bytes? (~> bytes->string/locale
              ->string)]
  [list? (~> ~a
             ->string)]
  [sequence? (~> ->list
                 ->string)]
  [generator? (~> ->list
                  ->string)]
  [else (~> ~a
            ->string)])

(define-switch ->number
  [number? _]
  [string? string->number]
  [char? char->integer]
  [(eq? ID) (reify 0 +)]
  [else (error '->number "Unsupported type ~a!" _)])

(define-switch ->inexact
  [(and number? inexact?) _]
  [number? exact->inexact]
  [else (~> ->number
            ->inexact)])

(define-switch ->exact
  [(and number? exact?) _]
  [number? inexact->exact]
  [else (~> ->number
            ->exact)])

(define (->integer v #:round [round 'down])
  (switch (v)
    [integer? _]
    [number? (switch
              [(gen (eq? round 'down))
               (~> floor
                   ->exact)]
              [(gen (eq? round 'up))
               (~> ceiling
                   ->exact)]
              [(gen (eq? round 'nearest))
               (~> b:round
                   ->exact)])]
    [else (~> ->number
              ->integer)]))

(define-switch ->list
  [list? _]
  [string? string->list]
  [vector? vector->list]
  [dict? dict->list]
  [set? set->list]
  [syntax? syntax->list]
  [bytes? bytes->list]
  [sequence? sequence->list]
  [generator? (~> ->stream
                  ->list)]
  [generic-set? set->list]
  [struct? (~> ->vector
               ->list)]
  [else (error '->list "Unsupported type ~a!" _)])

(define-switch ->vector
  [vector? (switch [immutable? _]
                   [else vector->immutable-vector])]
  [list? (~> list->vector
             ->vector)]
  [sequence? (~> ->list
                 ->vector)]
  [struct? (~> struct->vector
               (vector-drop 1)
               ->vector)]
  [else (~> ->list
            ->vector)])

(define-switch ->symbol
  [symbol? _]
  [string? string->symbol]
  [else (~> ->string
            ->symbol)])

(define-switch ->keyword
  [keyword? _]
  [string? string->keyword]
  [else (~> ->string
            ->keyword)])

(define-switch ->bytes
  [bytes? _]
  [list? list->bytes]
  [string? (~>> ->list
                (map char->integer)
                ->bytes)]
  [else (~> ->string
            ->bytes)])

(define-switch ->char
  [char? _]
  [integer? integer->char]
  [(and non-empty-string?
        (esc (string-len/c 2)))
   (string-ref 0)]
  [(and (esc (non-empty-listof any/c))
        (esc (property/c length (=/c 1))))
   (~> (list-ref 0)
       ->char)]
  [symbol? (~> ->string
               ->char)]
  [else (error '->char "Unsupported type ~a!" _)])

(define-switch ->stream
  [stream? _]
  [sequence? sequence->stream]
  [generator? (~> (in-producer (void))
                  ->stream)]
  [else (error '->stream "Unsupported type ~a!" _)])

(define (sequence->generator seq [return (void)])
  (generator ()
    (for-each (λ (v)
                (yield v)
                (void))
              seq)
    return))

(define (->generator v [return (void)])
  (switch (v)
    [generator? _]
    [sequence? (sequence->generator return)]
    [else (error '->generator "Unsupported type ~a!" _)]))

(define-switch ->set
  [set? _]
  [list? list->set]
  [else (~> ->list
            ->set)])

(define (->syntax v [ctx #f])
  (switch (v)
    [syntax? _]
    [else (datum->syntax (if ctx ctx #f) _)]))

(define-switch ->symex
  [syntax? syntax->datum]
  [else _])

(define (string->symex v)
  (read (open-input-string v)))

(define-switch ->values
  [vector? vector->values]
  [else (~> ->vector
            ->values)])

(define-switch ->hash
  [hash? _]
  [dict? make-immutable-hash]
  [(eq? ID) (reify (hash))]
  [else (error '->hash "Unsupported type ~a!" _)])

(define-switch (->procedure v)
  [procedure? _]
  [else (gen (thunk* v))])

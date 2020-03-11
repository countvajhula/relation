#lang racket/base

(require (prefix-in b: racket/base)
         racket/set
         racket/contract/base
         racket/generic
         (only-in algebraic/prelude
                  flip
                  ||
                  &&)
         racket/function
         racket/stream
         (only-in racket/list
                  group-by
                  splitf-at)
         data/collection
         lens)

(require "private/util.rkt")

(provide gen:orderable
         orderable/c
         (contract-out
          [orderable? (-> any/c boolean?)]
          [< (->* (orderable?)
                  (#:key (or/c (-> orderable? orderable?)
                               #f))
                  #:rest (listof orderable?)
                  boolean?)]
          [<= (->* (orderable?)
                   (#:key (or/c (-> orderable? orderable?)
                                #f))
                   #:rest (listof orderable?)
                   boolean?)]
          [≤ (->* (orderable?)
                  (#:key (or/c (-> orderable? orderable?)
                               #f))
                  #:rest (listof orderable?)
                  boolean?)]
          [>= (->* (orderable?)
                   (#:key (or/c (-> orderable? orderable?)
                                #f))
                   #:rest (listof orderable?)
                   boolean?)]
          [≥ (->* (orderable?)
                  (#:key (or/c (-> orderable? orderable?)
                               #f))
                  #:rest (listof orderable?)
                  boolean?)]
          [> (->* (orderable?)
                  (#:key (or/c (-> orderable? orderable?)
                               #f))
                  #:rest (listof orderable?)
                  boolean?)]
          (min (->* (orderable?)
                    (#:key (or/c (-> orderable? orderable?)
                                 #f))
                    #:rest (listof orderable?)
                    orderable?))
          (max (->* (orderable?)
                    (#:key (or/c (-> orderable? orderable?)
                                 #f))
                    #:rest (listof orderable?)
                    orderable?))))

(define-generics orderable
  (less-than? orderable other)
  (less-than-or-equal? orderable other)
  (greater-than-or-equal? orderable other)
  (greater-than? orderable other)
  #:fast-defaults ([number?
                    (define less-than? b:<)
                    (define less-than-or-equal? b:<=)
                    (define greater-than-or-equal? b:>=)
                    (define greater-than? b:>)]
                   [string?
                    (define less-than? string<?)
                    (define less-than-or-equal? string<=?)
                    (define greater-than-or-equal? string>=?)
                    (define greater-than? string>?)]
                   [bytes?
                    (define less-than? bytes<?)
                    (define less-than-or-equal?
                      (|| bytes=?
                          bytes<?))
                    (define greater-than-or-equal?
                      (|| bytes=?
                          bytes>?))
                    (define greater-than? bytes>?)]
                   [char?
                    (define less-than? char<?)
                    (define less-than-or-equal? char<=?)
                    (define greater-than-or-equal? char>=?)
                    (define greater-than? char>?)]
                   [set?
                    (define less-than? proper-subset?)
                    (define less-than-or-equal? subset?)
                    (define greater-than-or-equal? (flip subset?))
                    (define greater-than? (flip proper-subset?))]))

(define (< #:key [key #f] . args)
  (if key
      (apply < (map key args))
      (check-pairwise less-than? args)))

(define (<= #:key [key #f] . args)
  (if key
      (apply <= (map key args))
      (check-pairwise less-than-or-equal? args)))

(define (>= #:key [key #f] . args)
  (if key
      (apply >= (map key args))
      (check-pairwise greater-than-or-equal? args)))

(define (> #:key [key #f] . args)
  (if key
      (apply > (map key args))
      (check-pairwise greater-than? args)))

(define (min #:key [key #f] . args)
  (first (sort args
               (curry < #:key key))))

(define (max #:key [key #f] . args)
  (first (sort args
               (curry > #:key key))))

(define ≤ <=)
(define ≥ >=)

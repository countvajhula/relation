#lang racket/base

(require (prefix-in b: racket/base)
         racket/set
         racket/contract/base
         racket/generic
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
  (less-than? orderable . others)
  (less-than-or-equal? orderable . others)
  (greater-than-or-equal? orderable . others)
  (greater-than? orderable . others)
  #:fast-defaults ([number?
                    (define (less-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise b:< vals)))
                    (define (less-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise b:<= vals)))
                    (define (greater-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise b:>= vals)))
                    (define (greater-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise b:> vals)))]
                   [string?
                    (define (less-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise string<? vals)))
                    (define (less-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise string<=? vals)))
                    (define (greater-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise string>=? vals)))
                    (define (greater-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise string>? vals)))]
                   [bytes?
                    (define (less-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise bytes<? vals)))
                    (define (less-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise (λ (a b)
                                          (or (bytes=? a b)
                                              (bytes<? a b)))
                                        vals)))
                    (define (greater-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise (λ (a b)
                                          (or (bytes=? a b)
                                              (bytes>? a b)))
                                        vals)))
                    (define (greater-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise bytes>? vals)))]
                   [char?
                    (define (less-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise char<? vals)))
                    (define (less-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise char<=? vals)))
                    (define (greater-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise char>=? vals)))
                    (define (greater-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise char>? vals)))]
                   [set?
                    (define (less-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise proper-subset? vals)))
                    (define (less-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise subset? vals)))
                    (define (greater-than-or-equal? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise subset?
                                        (reverse vals))))
                    (define (greater-than? orderable . others)
                      (let ([vals (cons orderable others)])
                        (check-pairwise proper-subset?
                                        (reverse vals))))]))

(define (< #:key [key #f] . args)
  (if key
      (apply < (map key args))
      (apply less-than? args)))

(define (<= #:key [key #f] . args)
  (if key
      (apply <= (map key args))
      (apply less-than-or-equal? args)))

(define (>= #:key [key #f] . args)
  (if key
      (apply >= (map key args))
      (apply greater-than-or-equal? args)))

(define (> #:key [key #f] . args)
  (if key
      (apply > (map key args))
      (apply greater-than? args)))

(define (min #:key [key #f] . args)
  (first (sort args
               (curry < #:key key))))

(define (max #:key [key #f] . args)
  (first (sort args
               (curry > #:key key))))

(define ≤ <=)
(define ≥ >=)

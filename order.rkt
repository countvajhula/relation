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
  (< #:key [key] orderable . others)
  (<= #:key [key] orderable . others)
  (>= #:key [key] orderable . others)
  (> #:key [key] orderable . others)
  #:fast-defaults ([number?
                    (define/generic generic-< <)
                    (define/generic generic-<= <=)
                    (define/generic generic->= >=)
                    (define/generic generic-> >)
                    (define (< #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-< (map key vals))
                            (check-pairwise b:< vals))))
                    (define (<= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-<= (map key vals))
                            (check-pairwise b:<= vals))))
                    (define (>= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic->= (map key vals))
                            (check-pairwise b:>= vals))))
                    (define (> #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-> (map key vals))
                            (check-pairwise b:> vals))))]
                   [string?
                    (define/generic generic-< <)
                    (define/generic generic-<= <=)
                    (define/generic generic->= >=)
                    (define/generic generic-> >)
                    (define (< #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-< (map key vals))
                            (check-pairwise string<? vals))))
                    (define (<= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-<= (map key vals))
                            (check-pairwise string<=? vals))))
                    (define (>= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic->= (map key vals))
                            (check-pairwise string>=? vals))))
                    (define (> #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-> (map key vals))
                            (check-pairwise string>? vals))))]
                   [bytes?
                    (define/generic generic-< <)
                    (define/generic generic-<= <=)
                    (define/generic generic->= >=)
                    (define/generic generic-> >)
                    (define (< #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-< (map key vals))
                            (check-pairwise bytes<? vals))))
                    (define (<= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-<= (map key vals))
                            (check-pairwise (λ (a b)
                                              (or (bytes=? a b)
                                                  (bytes<? a b)))
                                            vals))))
                    (define (>= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic->= (map key vals))
                            (check-pairwise (λ (a b)
                                              (or (bytes=? a b)
                                                  (bytes>? a b)))
                                            vals))))
                    (define (> #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-> (map key vals))
                            (check-pairwise bytes>? vals))))]
                   [char?
                    (define/generic generic-< <)
                    (define/generic generic-<= <=)
                    (define/generic generic->= >=)
                    (define/generic generic-> >)
                    (define (< #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-< (map key vals))
                            (check-pairwise char<? vals))))
                    (define (<= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-<= (map key vals))
                            (check-pairwise char<=? vals))))
                    (define (>= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic->= (map key vals))
                            (check-pairwise char>=? vals))))
                    (define (> #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-> (map key vals))
                            (check-pairwise char>? vals))))]
                   [set?
                    (define/generic generic-< <)
                    (define/generic generic-<= <=)
                    (define/generic generic->= >=)
                    (define/generic generic-> >)
                    (define (< #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-< (map key vals))
                            (check-pairwise proper-subset? vals))))
                    (define (<= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-<= (map key vals))
                            (check-pairwise subset? vals))))
                    (define (>= #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic->= (map key vals))
                            (check-pairwise subset?
                                            (reverse vals)))))
                    (define (> #:key [key #f] orderable . others)
                      (let ([vals (cons orderable others)])
                        (if key
                            (apply generic-> (map key vals))
                            (check-pairwise proper-subset?
                                            (reverse vals)))))]))

(define (min #:key [key #f] . args)
  (first (sort args
               (curry < #:key key))))

(define (max #:key [key #f] . args)
  (first (sort args
               (curry > #:key key))))

(define ≤ <=)
(define ≥ >=)

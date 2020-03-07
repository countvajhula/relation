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

;; There is a lot of redundancy in this module that should ideally be addressed:
;;   1. contracts are duplicated across aliases
;;   2. transformation by key is repeated across
;;      all relations because the dispatch is by
;;      type and not the other arguments
;;   3. (for the same reason) generic versions of
;;      relations are redefined in every block
;; One alternative is to use separate operators for keyed- and
;; unkeyed relations, but that involves duplication of its own

(provide gen:comparable
         comparable/c
         (contract-out
          [comparable? (-> any/c boolean?)]
          [= (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [/= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [≠ (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [!= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [=/classes (->* ((listof comparable?))
                          (#:key (or/c (-> comparable? comparable?)
                                       #f))
                          (listof list?))]
          (generic-set (->* (comparable?)
                            (#:key (or/c (-> comparable? comparable?)
                                         #f))
                            #:rest (listof comparable?)
                            generic-set?))))

(define-generics comparable
  (= #:key [key] comparable . others)
  #:fallbacks [(define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))]
  #:defaults ([number?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise b:= vals))))]
              [string?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise string=? vals))))]
              [bytes?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise bytes=? vals))))]
              [char?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise char=? vals))))]
              [set?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))]
              [symbol?
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise eq? vals))))]
              [any/c
               (define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))]))

(define (/= #:key [key #f] . args)
  (not (apply = #:key key args)))

(define (=/classes #:key [key #f] collection)
  (let ([key (or key identity)])
    (group-by key collection =)))

(struct gset (contents key)
  #:transparent
  #:guard
  (λ (contents key type-name)
    (let ([classes (=/classes #:key key contents)])
      (if (empty? classes)
          (values (list) key)
          (values (stream->list (map first classes))
                  key))))
  #:methods gen:set
  [(define (set-member? st v)
     (let ([result (member v (gset-contents st) =)])
       (and result #t)))
   (define (set-add st v)
     (let ([contents (gset-contents st)]
           [key (gset-key st)])
       (apply generic-set #:key key v contents)))
   (define (set-remove st v)
     (let ([contents (gset-contents st)]
           [key (gset-key st)])
       (if (empty? contents)
           (apply generic-set #:key key contents)
           (let-values ([(before after)
                         (splitf-at contents
                                    (λ (x)
                                      (/= x v)))])
             (apply generic-set
                    #:key key
                    (append before (rest after)))))))
   (define (set->list st)
     (gset-contents st))
   (define (set->stream st)
     (sequence->stream (gset-contents st)))]
  #:methods gen:collection
  [(define (conj st v)
     (set-add st v))]
  #:methods gen:countable
  [(define/generic generic-length length)
   (define (length st)
     (generic-length (set->stream st)))]
  #:methods gen:sequence
  [(define/generic generic-empty? empty?)
   (define/generic generic-first first)
   (define/generic generic-rest rest)
   (define (empty? st)
     (generic-empty? (set->stream st)))
   (define (first st)
     (generic-first (set->stream st)))
   (define (rest st)
     (generic-rest (set->stream st)))])

(define (generic-set #:key [key #f] . args)
  (gset args key))

(define ≠ /=)
(define != /=)

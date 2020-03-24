#lang racket/base

(require (prefix-in b: racket/base)
         racket/set
         racket/contract/base
         racket/generic
         racket/function
         racket/stream
         (only-in racket/list
                  (group-by b:group-by)
                  splitf-at)
         data/collection)

(require "private/util.rkt")

(provide gen:comparable
         comparable/c
         (contract-out
          [comparable? (-> any/c boolean?)]
          [= (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [≠ (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [/= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [!= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [group-by (->* ((listof comparable?))
                         (#:key (or/c (-> comparable? comparable?)
                                      #f))
                         (listof list?))]
          [=/classes (->* ((listof comparable?))
                          (#:key (or/c (-> comparable? comparable?)
                                       #f))
                          (listof list?))]
          (generic-set (->* ()
                            (#:key (or/c (-> comparable? comparable?)
                                         #f))
                            #:rest (listof comparable?)
                            generic-set?))))

(define-generics comparable
  (equal? comparable other)
  #:fast-defaults ([number?
                    (define equal? b:=)]
                   [string?
                    (define equal? string=?)]
                   [bytes?
                    (define equal? bytes=?)]
                   [char?
                    (define equal? char=?)]
                   [symbol?
                    (define equal? eq?)])
  #:defaults ([any/c
               (define equal? b:equal?)]))

(define (= #:key [key #f] . args)
  (if key
      (apply = (map key args))
      (with-handlers ([exn:fail:contract? (λ (err) #f)])
        (check-pairwise equal? args))))

(define (group-by #:key [key #f] collection)
  (let ([key (or key identity)])
    (b:group-by key collection =)))

(define (≠ #:key [key #f] . args)
  (not (apply = #:key key args)))

(struct gset (contents key)
  #:transparent
  #:guard
  (λ (contents key type-name)
    (let ([classes (group-by #:key key contents)])
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
                                      (≠ x v)))])
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

(define /= ≠)
(define != ≠)
(define =/classes group-by)

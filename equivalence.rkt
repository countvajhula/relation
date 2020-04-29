#lang racket/base

(require (prefix-in b: racket/base)
         racket/set
         racket/contract/base
         racket/generic
         racket/function
         racket/stream
         racket/class
         (only-in racket/list
                  (group-by b:group-by)
                  splitf-at)
         data/collection
         (only-in algebraic/prelude
                  &&
                  ||))

(require "private/util.rkt")

(provide gen:comparable
         comparable/c
         (contract-out
          [comparable? (-> any/c boolean?)]
          [gset? (-> any/c boolean?)]
          [hash-code (-> comparable? fixnum?)]
          [secondary-hash-code (-> comparable? fixnum?)]
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
                            generic-set?))
          (tail (->* (comparable? sequence?)
                     (#:key (or/c (-> comparable? comparable?)
                                  #f))
                     sequence?))
          (member? (->* (comparable? sequence?)
                        (#:key (or/c (-> comparable? comparable?)
                                     #f))
                        boolean?))))

(define-generics comparable
  (equal? comparable other)
  (hash-code comparable)
  (secondary-hash-code comparable)
  #:fallbacks [(define equal? b:equal?)
               (define hash-code equal-hash-code)
               (define secondary-hash-code equal-secondary-hash-code)]
  #:fast-defaults ([number?
                    (define equal? b:=)
                    (define hash-code values)]
                   [symbol?
                    (define equal? eq?)
                    (define hash-code eq-hash-code)])
  #:defaults ([string?
               (define equal? b:equal?)
               (define hash-code equal-hash-code)
               (define secondary-hash-code equal-secondary-hash-code)]
              [sequence?
               (define/generic generic-equal? equal?)
               (define/generic generic-hash-code hash-code)
               (define (equal? comparable other)
                 (cond [(for-all empty? (list comparable other))
                        #t]
                       [(exists empty? (list comparable other))
                        #f]
                       [else (let ([a (first comparable)]
                                   [b (first other)])
                               (and (generic-equal? a b)
                                    (equal? (rest comparable)
                                            (rest other))))]))
               (define (hash-code comparable)
                 (let ([hashes (map generic-hash-code comparable)]
                       [squares (map (curryr expt 2) (naturals 1))])
                   (apply +
                          (equal-hash-code sequence?)
                          (zip-with *
                                    hashes
                                    squares))))]
              [any/c
               (define equal? b:equal?)
               (define hash-code equal-hash-code)
               (define secondary-hash-code equal-secondary-hash-code)]))

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
     (let* ([key (gset-key st)]
            [result (member v
                            (gset-contents st)
                            (curry = #:key key))])
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
                                      (≠ #:key key x v)))])
             (apply generic-set
                    #:key key
                    (append before
                            (if (empty? after)
                                after
                                (rest after))))))))
   (define (set=? st st2)
     (let ([key (gset-key st)]
           [contents (gset-contents st)]
           [key2 (gset-key st2)]
           [contents2 (gset-contents st2)])
       (and (= key key2)
            (= #:key (compose (curry apply set)
                              (if key
                                  (curry map key)
                                  identity))
               contents
               contents2))))
   (define (set->list st)
     (gset-contents st))
   (define (set->stream st)
     (sequence->stream (gset-contents st)))]
  #:methods gen:equal+hash
  [(define (equal-proc st other equal-recur)
     (set=? st other))
   (define (hash-proc st hash-recur)
     (+ (* 3 (equal-hash-code (gset-key st)))
        (equal-hash-code (apply set (gset-contents st)))))
   (define (hash2-proc st hash2-recur)
     (+ (equal-hash-code (gset-key st))
        (equal-secondary-hash-code (apply set (gset-contents st)))))]
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

(define (tail #:key [key #f] elem col)
  (if ((|| set?
           gset?) col)
      (raise-argument-error 'tail
                            "sequence? that is not a pure set"
                            col)
      (if (empty? col)
          null
          (if (= #:key key
                 elem
                 (first col))
              col
              (tail #:key key
                    elem
                    (rest col))))))

(define (member? #:key [key #f] elem col)
  (if (gset? col)
      (set-member? col elem)
      (not (null? (tail #:key key
                        elem
                        (in col))))))

(define /= ≠)
(define != ≠)
(define =/classes group-by)

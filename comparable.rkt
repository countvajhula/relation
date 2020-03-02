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
          [< (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [<= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [≤ (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [= (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [>= (->* (comparable?)
                   (#:key (or/c (-> comparable? comparable?)
                                #f))
                   #:rest (listof comparable?)
                   boolean?)]
          [≥ (->* (comparable?)
                  (#:key (or/c (-> comparable? comparable?)
                               #f))
                  #:rest (listof comparable?)
                  boolean?)]
          [> (->* (comparable?)
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
                            generic-set?))
          (min (->* (comparable?)
                    (#:key (or/c (-> comparable? comparable?)
                                 #f))
                    #:rest (listof comparable?)
                    comparable?))
          (max (->* (comparable?)
                    (#:key (or/c (-> comparable? comparable?)
                                 #f))
                    #:rest (listof comparable?)
                    comparable?))))

(define (check-pairwise check? vals)
  (let ([current (car vals)]
        [remaining (cdr vals)])
    (if (empty? remaining)
        #t
        (and (check? current (car remaining))
             (check-pairwise check? remaining)))))

(define-generics comparable
  (< #:key [key] comparable . others)
  (<= #:key [key] comparable . others)
  (= #:key [key] comparable . others)
  (>= #:key [key] comparable . others)
  (> #:key [key] comparable . others)
  #:fallbacks [(define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))
               (define (< #:key [key #f] comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (<= #:key [key #f] comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (>= #:key [key #f] comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (> #:key [key #f] comparable . others)
                 (error "Type is not orderable!" comparable))]
  #:defaults ([number?
               (define/generic generic-= =)
               (define/generic generic-< <)
               (define/generic generic-<= <=)
               (define/generic generic->= >=)
               (define/generic generic-> >)
               (define (< #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-< (map key vals))
                       (check-pairwise b:< vals))))
               (define (<= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-<= (map key vals))
                       (check-pairwise b:<= vals))))
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise b:= vals))))
               (define (>= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic->= (map key vals))
                       (check-pairwise b:>= vals))))
               (define (> #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-> (map key vals))
                       (check-pairwise b:> vals))))]
              [string?
               (define/generic generic-= =)
               (define/generic generic-< <)
               (define/generic generic-<= <=)
               (define/generic generic->= >=)
               (define/generic generic-> >)
               (define (< #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-< (map key vals))
                       (check-pairwise string<? vals))))
               (define (<= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-<= (map key vals))
                       (check-pairwise string<=? vals))))
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise string=? vals))))
               (define (>= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic->= (map key vals))
                       (check-pairwise string>=? vals))))
               (define (> #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-> (map key vals))
                       (check-pairwise string>? vals))))]
              [bytes?
               (define/generic generic-= =)
               (define/generic generic-< <)
               (define/generic generic-<= <=)
               (define/generic generic->= >=)
               (define/generic generic-> >)
               (define (< #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-< (map key vals))
                       (check-pairwise bytes<? vals))))
               (define (<= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-<= (map key vals))
                       (check-pairwise (λ (a b)
                                         (or (bytes=? a b)
                                             (bytes<? a b)))
                                       vals))))
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise bytes=? vals))))
               (define (>= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic->= (map key vals))
                       (check-pairwise (λ (a b)
                                         (or (bytes=? a b)
                                             (bytes>? a b)))
                                       vals))))
               (define (> #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-> (map key vals))
                       (check-pairwise bytes>? vals))))]
              [char?
               (define/generic generic-= =)
               (define/generic generic-< <)
               (define/generic generic-<= <=)
               (define/generic generic->= >=)
               (define/generic generic-> >)
               (define (< #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-< (map key vals))
                       (check-pairwise char<? vals))))
               (define (<= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-<= (map key vals))
                       (check-pairwise char<=? vals))))
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise char=? vals))))
               (define (>= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic->= (map key vals))
                       (check-pairwise char>=? vals))))
               (define (> #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-> (map key vals))
                       (check-pairwise char>? vals))))]
              [set?
               (define/generic generic-= =)
               (define/generic generic-< <)
               (define/generic generic-<= <=)
               (define/generic generic->= >=)
               (define/generic generic-> >)
               (define (< #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-< (map key vals))
                       (check-pairwise proper-subset? vals))))
               (define (<= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-<= (map key vals))
                       (check-pairwise subset? vals))))
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))
               (define (>= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic->= (map key vals))
                       (check-pairwise subset?
                                       (reverse vals)))))
               (define (> #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-> (map key vals))
                       (check-pairwise proper-subset?
                                       (reverse vals)))))]
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

(define (min #:key [key #f] . args)
  (first (sort args
               (curry < #:key key))))

(define (max #:key [key #f] . args)
  (first (sort args
               (curry > #:key key))))

(define ≤ <=)
(define ≥ >=)
(define ≠ /=)
(define != /=)

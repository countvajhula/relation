#lang racket

(require (prefix-in b: racket/base)
         racket/generic
         racket/function)

(provide gen:comparable
         comparable/c
         (contract-out
          [comparable? (-> any/c boolean?)]
          [< (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [<= (->* (comparable?)
                   (#:key (-> comparable? comparable?))
                   #:rest (listof comparable?)
                   boolean?)]
          [≤ (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [= (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [/= (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [≠ (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [>= (->* (comparable?)
                   (#:key (-> comparable? comparable?))
                   #:rest (listof comparable?)
                   boolean?)]
          [≥ (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]
          [> (->* (comparable?)
                  (#:key (-> comparable? comparable?))
                  #:rest (listof comparable?)
                  boolean?)]))

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
  (/= #:key [key] comparable . others)
  (>= #:key [key] comparable . others)
  (> #:key [key] comparable . others)
  #:fallbacks [(define/generic generic-= =)
               (define (= #:key [key #f] comparable . others)
                 (let ([vals (cons comparable others)])
                   (if key
                       (apply generic-= (map key vals))
                       (check-pairwise equal? vals))))
               (define (/= #:key [key #f] comparable . others)
                 (not (apply generic-= #:key key comparable others)))
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

(define ≤ <=)
(define ≥ >=)
(define ≠ /=)

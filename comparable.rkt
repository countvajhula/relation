#lang racket

(require (prefix-in b: racket/base)
         racket/generic)

(provide gen:comparable
         comparable/c
         (contract-out
          [comparable? (-> any/c boolean?)]
          [< (-> comparable? comparable? ... boolean?)]
          [<= (-> comparable? comparable? ... boolean?)]
          [= (-> comparable? comparable? ... boolean?)]
          [=~ (-> (-> comparable? comparable?)
                  comparable?
                  comparable?
                  ...
                  boolean?)]
          [≡ (-> (-> comparable? comparable?)
                 comparable?
                 comparable?
                 ...
                 boolean?)]
          [/= (-> comparable? comparable? ... boolean?)]
          [>= (-> comparable? comparable? ... boolean?)]
          [> (-> comparable? comparable? ... boolean?)]
          [≤ (-> comparable? comparable? ... boolean?)]
          [≥ (-> comparable? comparable? ... boolean?)]))

(define (check-sequence check? vals)
  (let ([current (car vals)]
        [remaining (cdr vals)])
    (if (empty? remaining)
        #t
        (and (check? current (car remaining))
             (check-sequence check? remaining)))))

(define-generics comparable
  (< comparable . others)
  (<= comparable . others)
  (= comparable . others)
  (=~ key comparable . others)
  (/= comparable . others)
  (>= comparable . others)
  (> comparable . others)
  #:fallbacks [(define/generic generic-= =)
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence equal? vals)))
               (define (=~ key comparable . others)
                 (let ([vals (map key (cons comparable others))])
                   (apply generic-= vals)))
               (define (/= comparable . others)
                 (not (apply generic-= comparable others)))
               (define (< comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (<= comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (>= comparable . others)
                 (error "Type is not orderable!" comparable))
               (define (> comparable . others)
                 (error "Type is not orderable!" comparable))]
  #:defaults ([number?
               (define (< comparable . others)
                 (apply b:< (cons comparable others)))
               (define (<= comparable . others)
                 (apply b:<= comparable others))
               (define (= comparable . others)
                 (apply b:= comparable others))
               (define (>= comparable . others)
                 (apply b:>= comparable others))
               (define (> comparable . others)
                 (apply b:> comparable others))]
              [string?
               (define (< comparable . others)
                 (apply string<? (cons comparable others)))
               (define (<= comparable . others)
                 (apply string<=? (cons comparable others)))
               (define (= comparable . others)
                 (apply string=? (cons comparable others)))
               (define (>= comparable . others)
                 (apply string>=? (cons comparable others)))
               (define (> comparable . others)
                 (apply string>? (cons comparable others)))]
              [char?
               (define (< comparable . others)
                 (apply char<? (cons comparable others)))
               (define (<= comparable . others)
                 (apply char<=? (cons comparable others)))
               (define (= comparable . others)
                 (apply char=? (cons comparable others)))
               (define (>= comparable . others)
                 (apply char>=? (cons comparable others)))
               (define (> comparable . others)
                 (apply char>? (cons comparable others)))]
              [set?
               (define/generic generic-<= <=)
               (define/generic generic-< <)
               (define/generic generic-= =)
               (define/generic generic-> >)
               (define (< comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence proper-subset? vals)))
               (define (<= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence subset? vals)))
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence equal? vals)))
               (define (>= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence subset?
                                   (reverse vals))))
               (define (> comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence proper-subset?
                                   (reverse vals))))]
              [symbol?
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence eq? vals)))]
              [any/c
               (define (= comparable . others)
                 (let ([vals (cons comparable others)])
                   (check-sequence equal? vals)))]))

(define ≤ <=)
(define ≥ >=)
(define ≡ =~)

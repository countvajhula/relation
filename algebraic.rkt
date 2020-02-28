#lang racket/base

(require (prefix-in b: racket/base)
         racket/contract/base
         racket/vector
         racket/set
         racket/dict
         (rename-in racket/function
                    (identity f:identity))
         racket/generic
         (rename-in data/collection
                    (foldl d:foldl))
         (only-in algebraic/prelude
                  flip)
         relation/comparable
         relation/transform)

(provide gen:composable
         composable/c
         gen:appendable
         appendable/c
         gen:monoid
         monoid/c
         gen:addable
         addable/c
         (contract-out
          [composable? (-> any/c boolean?)]
          [>< (-> composable? composable? composable?)]
          [: (-> composable? composable? composable?)]
          [appendable? (-> any/c boolean?)]
          [.. (-> appendable? appendable? ... appendable?)]
          [∘ (-> appendable? appendable? ... appendable?)]
          [monoid? (-> any/c boolean?)]
          [identity (-> monoid? (-> any/c any/c any/c) monoid?)]
          [addable? (-> any/c boolean?)]
          [inverse (-> addable? (-> any/c any/c any/c) addable?)]
          [+ (-> addable? addable? ... addable?)]
          [- (-> addable? addable? ... addable?)]
          [foldl (->* ((-> any/c any/c any/c) (listof any/c))
                      (any/c)
                      any/c)]
          [foldr (->* ((-> any/c any/c any/c) (listof any/c))
                      (any/c)
                      any/c)]
          [fold (->* ((-> any/c any/c any/c) (listof any/c))
                     (any/c)
                     any/c)]))

(define-generics composable
  ;; "Magma"
  ;; This is the most general form of composition, making no
  ;; assumptions about properties the operation must satisfy
  (>< composable other)
  #:fallbacks
  [(define (>< composable other)
     (cons composable other))]
  #:defaults
  ([any/c]))

(define-generics appendable
  ;; "Semigroup"
  ;; concatenation-like operation
  (.. appendable . others)
  #:defaults ([number?
               (define (.. appendable . others)
                 (apply b:* appendable others))]
              [string?
               (define (.. appendable . others)
                 (apply string-append appendable others))]
              [bytes?
               (define (.. appendable . others)
                 (apply bytes-append appendable others))]
              [list?
               (define (.. appendable . others)
                 (apply b:append appendable others))]
              [vector?
               (define (.. appendable . others)
                 (apply vector-append appendable others))]
              [set?
               (define (.. appendable . others)
                 (apply set-union appendable others))]
              [dict?
               (define (.. appendable . others)
                 (make-hash (->list (apply append appendable others))))]
              [sequence?
               (define (.. appendable . others)
                 (apply append appendable others))]
              [procedure?
               (define (.. appendable . others)
                 (apply compose appendable others))]))

(define-generics monoid
  (identity monoid operation)
  #:defaults
  ([number?
    (define (identity monoid operation)
      (cond [(= operation +) 0]
            [(= operation *) 1]
            [(= operation ..) 1]
            [else (error "Operation not recognized!" operation)]))]
   [procedure?
    (define (identity monoid operation)
      f:identity)]
   [string?
    (define (identity monoid operation)
      "")]
   [bytes?
    (define (identity monoid operation)
      #"")]
   [list?
    (define (identity monoid operation)
      (list))]
   [vector?
    (define/generic g-identity identity)
    (define (identity monoid operation)
      (cond [(= operation ..) #()]
            [(= operation *)
             (error "Operation not supported!" operation)]
            [(= operation +)
             (->vector
              (take (length monoid)
                    (repeat (g-identity (first monoid)
                                        +))))]))]
   [set?
    (define (identity monoid operation)
      (set))]
   [dict?
    (define (identity monoid operation)
      (hash))]
   [sequence?
    (define (identity monoid operation)
      (list))]))

(define-generics addable
  (inverse addable operation)
  (+ addable . others)
  (- addable . others)
  #:fallbacks [(define/generic g-+ +)
               (define/generic g-inverse inverse)
               (define (inverse addable operation)
                 (error "Unrecognized addable type!" addable))
               (define (+ addable . others)
                 (error "Unrecognized addable type!" addable))
               (define (- addable . others)
                 (if (empty? others)
                     (g-inverse addable g-+)
                     (let ([minus (curryr g-inverse g-+)])
                       (apply g-+ addable (map minus others)))))]
  #:defaults ([number?
               (define/generic g-+ +)
               (define/generic g-- -)
               (define (inverse addable operation)
                 (cond [(= operation g-+)
                        (b:- addable)]
                       [(= operation *)
                        (b:/ 1 addable)]
                       [else (error "Unsupported addable operation!" operation)]))
               (define (+ addable . others)
                 (apply b:+ addable others))]
              [vector?
               (define/generic g-+ +)
               (define/generic g-inverse inverse)
               (define (inverse addable operation)
                 (->vector
                  (if (empty? addable)
                      addable
                      (let ([minus (curryr g-inverse operation)])
                        (map minus addable)))))
               (define (+ addable . others)
                 (->vector
                  (apply map g-+ addable others)))]))

(define : ><)
(define ∘ ..)

(define (foldl f vs [base #f])
  (if base
      (d:foldl (flip f) base vs)
      (let ([id (identity (first vs) f)])
        (d:foldl (flip f) id vs))))

(define (foldr f vs [base #f])
  (foldl f (reverse vs) base))

(define fold foldr)

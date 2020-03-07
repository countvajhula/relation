#lang at-exp racket

(require (prefix-in b: racket/base)
         racket/contract/base
         racket/vector
         racket/set
         racket/dict
         racket/function
         racket/generic
         (rename-in data/collection
                    (foldl d:foldl))
         (only-in algebraic/prelude
                  flip)
         relation/equivalence
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
          [appendable? (-> any/c boolean?)]
          [.. (-> appendable? appendable? ... appendable?)]
          [∘ (-> appendable? appendable? ... appendable?)]
          [monoid? (-> any/c boolean?)]
          [id (-> monoid? (-> any/c any/c any/c) monoid?)]
          [addable? (-> any/c boolean?)]
          [inverse (-> addable? (-> any/c any/c any/c) addable?)]
          [+ (-> addable? addable? ... addable?)]
          [- (-> addable? addable? ... addable?)]
          [foldl (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c)
                      any/c)]
          [foldr (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c)
                      any/c)]
          [fold (->* ((-> any/c any/c any/c) (sequenceof any/c))
                     (any/c)
                     any/c)]))

(define-generics composable
  ;; "Magma"
  ;; This is the most general form of composition, making no
  ;; assumptions about properties the operation must satisfy
  ;; except that it satisfy closure, i.e. the composition
  ;; yields an instance of the same type as the inputs
  (>< composable other)
  #:fallbacks
  [(define (>< composable other)
     (cons composable other))]
  #:defaults
  ([number?
    (define (>< composable other)
      (b:+ composable other))]
   [string?
    (define (>< composable other)
      (string-append composable other))]
   [bytes?
    (define (>< composable other)
      (bytes-append composable other))]
   [list?
    (define (>< composable other)
      (b:append composable other))]
   [vector?
    (define (>< composable other)
      (vector-append composable other))]
   [set?
    (define (>< composable other)
      (set-union composable other))]
   [dict?
    (define (>< composable other)
      (make-hash (->list (append composable other))))]
   [sequence?
    (define (>< composable other)
      (append composable other))]
   [procedure?
    (define (>< composable other)
      (compose composable other))]
   [any/c]))

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
  (id monoid operation)
  #:defaults
  ([number?
    (define (id monoid operation)
      (cond [(= operation +) 0]
            [(= operation *) 1]
            [(= operation ..) 1]
            [else (error "Operation not recognized!" operation)]))]
   [procedure?
    (define (id monoid operation)
      identity)]
   [string?
    (define (id monoid operation)
      "")]
   [bytes?
    (define (id monoid operation)
      #"")]
   [list?
    (define (id monoid operation)
      (list))]
   [vector?
    (define/generic generic-id id)
    (define (id monoid operation)
      (cond [(= operation ..) #()]
            [(= operation *)
             (error "Operation not supported!" operation)]
            [(= operation +)
             (->vector
              (take (length monoid)
                    (repeat (generic-id (first monoid)
                                        +))))]))]
   [set?
    (define (id monoid operation)
      (set))]
   [dict?
    (define (id monoid operation)
      (hash))]
   [sequence?
    (define (id monoid operation)
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

(define ∘ ..)

(define (foldl f vs [base #f])
  (if base
      (d:foldl (flip f) base vs)
      (if (empty? vs)
          (error @~a{Input sequence is empty and no base value was provided!
                     Available data is insufficient to compute a result.})
          (let ([id-element (id (first vs) f)])
            (d:foldl (flip f) id-element vs)))))

(define (foldr f vs [base #f])
  (foldl f (reverse vs) base))

(define fold foldr)

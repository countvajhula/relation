#lang racket

(require (prefix-in b: racket/base)
         racket/vector
         racket/set
         racket/dict
         (prefix-in f: racket/function)
         racket/generic
         data/collection
         relation/comparable
         relation/transform)

(provide gen:composable
         composable/c
         gen:appendable
         appendable/c
         gen:monoid
         monoid/c
         gen:group
         group/c
         (contract-out
          [composable? (-> any/c boolean?)]
          [>< (-> composable? composable? composable?)]
          [appendable? (-> any/c boolean?)]
          [.. (-> appendable? appendable? ... appendable?)]
          [∘ (-> appendable? appendable? ... appendable?)]
          [monoid? (-> any/c boolean?)]
          [identity (-> monoid? (-> any/c any/c any/c) monoid?)]
          [group? (-> any/c boolean?)]
          [inverse (-> group? (-> any/c any/c any/c) group?)]
          [+ (-> group? group? ... group?)]
          [- (-> group? group? ... group?)]
          [fold (->* ((-> any/c any/c any/c) (listof any/c))
                     (any/c)
                     any/c)]))

(define-generics composable
  ;; "Magma"
  ;; This is the most general form of composition, making no
  ;; assumptions about properties the operation must satisfy
  (>< composable other)
  #:defaults
  ([number?
    (define (>< composable other)
      (b:+ composable other))]))

(define-generics appendable
  ;; "Semigroup"
  ;; concatenation-like operation
  ;; it may make sense to define the chained operation in terms
  ;; of the composable one, as a fold
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
            [else (error "Operation not recognized!")]))]
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
             (error "Operation not supported!")]
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

(define-generics group
  (inverse group operation)
  (+ group . others)
  (- group . others)
  #:fallbacks [(define/generic g-+ +)
               (define/generic g-inverse inverse)
               (define (inverse group operation)
                 (error "Unrecognized group type!" group))
               (define (+ group . others)
                 (error "Unrecognized group type!" group))
               (define (- group . others)
                 (if (empty? others)
                     (g-inverse group g-+)
                     (let ([minus (f:curryr g-inverse g-+)])
                       (apply g-+ group (map minus others)))))]
  #:defaults ([number?
               (define/generic g-+ +)
               (define/generic g-- -)
               (define (inverse group operation)
                 (cond [(= operation g-+)
                        (b:- group)]
                       [(= operation *)
                        (b:/ 1 group)]))
               (define (+ group . others)
                 (apply b:+ group others))]
              [vector?
               (define/generic g-+ +)
               (define/generic g-inverse inverse)
               (define (inverse group operation)
                 (->vector
                  (if (empty? group)
                      group
                      (let ([minus (f:curryr g-inverse operation)])
                        (map minus group)))))
               (define (+ group . others)
                 (->vector
                  (apply map g-+ group others)))]))

(define ∘ ..)

(define (fold f vs [base #f])
  (if base
      (foldr f base vs)
      (let ([id (identity (first vs) f)])
        (foldr f id vs))))

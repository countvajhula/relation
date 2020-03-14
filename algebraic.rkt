#lang at-exp racket

(require (prefix-in b: racket/base)
         racket/contract/base
         racket/vector
         racket/set
         racket/dict
         racket/function
         racket/generic
         (rename-in data/collection
                    (foldl d:foldl)
                    (foldl/steps d:foldl/steps)
                    (append d:append))
         (only-in algebraic/prelude
                  flip)
         relation/equivalence
         relation/transform)

(provide gen:appendable
         appendable/c
         gen:multipliable
         multipliable/c
         gen:addable
         addable/c
         (contract-out
          [appendable? (-> any/c boolean?)]
          [append (-> appendable? appendable? appendable?)]
          [appendable-identity (-> appendable? appendable?)]
          [multipliable? (-> any/c boolean?)]
          [multiply (-> multipliable? multipliable? multipliable?)]
          [multipliable-identity (-> multipliable? multipliable?)]
          [multipliable-inverse (-> multipliable? multipliable?)]
          [add (-> addable? addable? addable?)]
          [addable-identity (-> addable? addable?)]
          [addable-inverse (-> addable? addable?)]
          [id (-> procedure? procedure?)]
          [inverse (-> procedure? procedure?)]
          [.. (-> appendable? appendable? ... appendable?)]
          [∘ (-> appendable? appendable? ... appendable?)]
          [* (-> multipliable? multipliable? ... multipliable?)]
          [/ (-> multipliable? multipliable? ... multipliable?)]
          [+ (-> addable? addable? ... addable?)]
          [- (-> addable? addable? ... addable?)]
          [foldl (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c)
                      any/c)]
          [foldl/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                            (any/c)
                            any/c)]
          [foldr (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c)
                      any/c)]
          [fold (->* ((-> any/c any/c any/c) (sequenceof any/c))
                     (any/c)
                     any/c)]
          [foldr/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                            (any/c)
                            any/c)]
          [fold/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                           (any/c)
                           any/c)]))

;; 5. get the current functionality working / tests to pass

;; alternatively, if types differ, then still allow it by
;; treating it as the "anekantic" type

;; compose is not a good name for this since composition is elementary
;; whereas function composition is specifically an append-like operation
(define-generics appendable
  (append appendable other)
  (appendable-identity appendable)
  (appendable-inverse appendable)
  #:fallbacks [(define (appendable-inverse appendable)
                 (error "Type is not invertible under append!"))]
  #:fast-defaults ([string?
                    (define append string-append)
                    (define (appendable-identity appendable)
                      "")]
                   [bytes?
                    (define append bytes-append)
                    (define (appendable-identity appendable)
                      #"")]
                   [list?
                    (define append b:append)
                    (define (appendable-identity appendable)
                      (list))]
                   [vector?
                    (define append vector-append)
                    (define (appendable-identity appendable)
                      #())]
                   [set?
                    (define append set-union)
                    (define (appendable-identity appendable)
                      (set))]
                   [dict?
                    (define append
                      (compose make-hash
                               ->list
                               d:append))
                    (define (appendable-identity appendable)
                      (hash))]
                   [sequence?
                    (define append d:append)
                    (define (appendable-identity appendable)
                      (list))]
                   [procedure?
                    (define append compose)
                    (define (appendable-identity appendable)
                      identity)]))

(define-generics multipliable
  (multiply multipliable other)
  (multipliable-identity multipliable)
  (multipliable-inverse multipliable)
  #:fallbacks [(define (multipliable-inverse multipliable)
                 (error "Type is not invertible under multiplication!"))]
  #:fast-defaults ([number?
                    (define multiply b:*)
                    (define (multipliable-identity multipliable)
                      1)
                    (define multipliable-inverse (curry b:/ 1))]))

(define-generics addable
  (add addable other)
  (addable-identity addable)
  (addable-inverse addable)
  #:fast-defaults ([number?
                    (define add b:+)
                    (define (addable-identity addable)
                      0)
                    (define addable-inverse b:-)]
                   [vector?
                    (define/generic generic-add add)
                    (define/generic generic-addable-inverse addable-inverse)
                    (define add (compose ->vector
                                         (curry map
                                                generic-add)))
                    (define (addable-identity addable)
                      (->vector
                       (take (length addable)
                             (repeat ((id +) (first addable))))))
                    (define addable-inverse (compose ->vector
                                                     (curry map
                                                            generic-addable-inverse)))]))

(define (id operation)
  (cond [(member operation (list + add))
         addable-identity]
        [(member operation (list * multiply))
         multipliable-identity]
        [(member operation (list .. append))
         appendable-identity]
        [else (error "Identity not defined for operation!")]))

(define (inverse operation)
  (cond [(member operation (list + add))
         addable-inverse]
        [(member operation (list * multiply))
         multipliable-inverse]
        [(member operation (list .. append))
         appendable-inverse]
        [else (error "Inverse not defined for operation!")]))

(define (.. v . remaining)
  (foldl append (cons v remaining) #:order 'bab))

(define (* v . remaining)
  (foldl multiply (cons v remaining) #:order 'bab))

(define (/ v . remaining)
  (if (empty? remaining)
      (multipliable-inverse v)
      (apply * v (map multipliable-inverse remaining))))

(define (+ v . remaining)
  (foldl add (cons v remaining) #:order 'bab))

(define (- v . remaining)
  (if (empty? remaining)
      (addable-inverse v)
      (apply + v (map addable-inverse remaining))))

(define ∘ ..)

(define (foldl f vs [base #f] #:order [order 'abb])
  (let ([fold-proc (cond [(= order 'abb)
                          (flip f)]
                         [(= order 'bab)
                          f]
                         [else (error "Invalid fold argument order!")])])
    (if base
        (d:foldl fold-proc
                 base
                 vs)
        (if (empty? vs)
            (error @~a{Input sequence is empty and no base value was provided!
                       Available data is insufficient to compute a result.})
            (let ([id-element ((id f) (first vs))])
              (d:foldl fold-proc
                       id-element
                       vs))))))

(define (foldl/steps f vs [base #f] #:order [order 'abb])
  (let ([fold-proc (cond [(= order 'abb)
                          (flip f)]
                         [(= order 'bab)
                          f]
                         [else (error "Invalid fold argument order!")])])
    (if base
        (d:foldl/steps fold-proc
                       base
                       vs)
        (if (empty? vs)
            (error @~a{Input sequence is empty and no base value was provided!
                       Available data is insufficient to compute a result.})
            (let ([id-element ((id f) (first vs))])
              (d:foldl/steps fold-proc
                             id-element
                             vs))))))

(define (foldr f vs [base #f] #:order [order 'abb])
  (foldl f
         (reverse vs)
         base
         #:order order))

(define (foldr/steps f vs [base #f] #:order [order 'abb])
  (foldl/steps f
               (reverse vs)
               base
               #:order order))

(define fold foldr)
(define fold/steps foldr/steps)

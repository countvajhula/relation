#lang at-exp racket

(require (prefix-in b: racket/base)
         racket/contract/base
         racket/vector
         racket/set
         racket/dict
         racket/function
         racket/generic
         racket/undefined
         (rename-in data/collection
                    (foldl d:foldl)
                    (foldl/steps d:foldl/steps)
                    (append d:append))
         (only-in algebraic/prelude
                  flip)
         point-free
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
          [appendable-inverse (-> appendable? appendable?)]
          [multipliable? (-> any/c boolean?)]
          [multiply (-> multipliable? multipliable? multipliable?)]
          [multipliable-identity (-> multipliable? multipliable?)]
          [multipliable-inverse (-> multipliable? multipliable?)]
          [addable? (-> any/c boolean?)]
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
          [fold (->* ((-> any/c any/c any/c) (sequenceof any/c))
                     (any/c #:order (one-of/c 'abb
                                              'bab)
                            #:direction (one-of/c 'left
                                                  'right)
                            #:with-steps boolean?)
                     any/c)]
          [foldl (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c #:order (one-of/c 'abb
                                               'bab)
                             #:with-steps boolean?)
                      any/c)]
          [foldr (->* ((-> any/c any/c any/c) (sequenceof any/c))
                      (any/c #:order (one-of/c 'abb
                                               'bab)
                             #:with-steps boolean?)
                      any/c)]
          [fold/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                           (any/c #:order (one-of/c 'abb
                                                    'bab)
                                  #:direction (one-of/c 'left
                                                        'right))
                           any/c)]
          [foldl/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                            (any/c #:order (one-of/c 'abb
                                                     'bab))
                            any/c)]
          [foldr/steps (->* ((-> any/c any/c any/c) (sequenceof any/c))
                            (any/c #:order (one-of/c 'abb
                                                     'bab))
                            any/c)]))

(define-generics appendable
  (append appendable other)
  (appendable-identity appendable)
  (appendable-inverse appendable)
  #:fallbacks [(define (appendable-inverse appendable)
                 (error 'appendable-inverse
                        "~a is not invertible under the append operation!"
                        appendable))]
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
                    (define/thrush append
                      d:append
                      ->list
                      make-immutable-hash)
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
                 (error 'multipliable-inverse
                        "~a is not invertible under the multiply operation!"
                        multipliable))]
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
                    (define/thrush add
                      (curry map
                             generic-add)
                      ->vector)
                    (define (addable-identity addable)
                      (->vector
                       (take (length addable)
                             (repeat ((id +) (first addable))))))
                    (define/thrush addable-inverse
                      (curry map
                             generic-addable-inverse)
                      ->vector)]))

(define (id operation)
  (cond [(member operation (list + add))
         addable-identity]
        [(member operation (list * multiply))
         multipliable-identity]
        [(member operation (list .. append))
         appendable-identity]
        [else (error 'id
                     "Identity not defined for operation ~a!"
                     operation)]))

(define (inverse operation)
  (cond [(member operation (list + add))
         addable-inverse]
        [(member operation (list * multiply))
         multipliable-inverse]
        [(member operation (list .. append))
         appendable-inverse]
        [else (error 'inverse
                     "Inverse not defined for operation ~a!"
                     operation)]))

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

(define (undefined? v)
  (eq? v undefined))

(define (fold f
              vs
              [base undefined]
              #:order [order 'abb]
              #:direction [direction 'right]
              #:with-steps [with-steps #f])
  (let ([combiner-proc (cond [(= order 'abb)
                              (flip f)]
                             [(= order 'bab)
                              f]
                             [else (error 'fold
                                          "Invalid fold argument order ~a!"
                                          order)])]
        [vs (cond [(= direction 'left) vs]
                  [(= direction 'right) (reverse vs)]
                  (error 'fold
                         "Invalid fold direction ~a!"
                         direction))]
        [fold-method (if with-steps
                         d:foldl/steps
                         d:foldl)])
    (if (undefined? base)
        (if (empty? vs)
            (error 'fold
                   @~a{Input sequence is empty and no base value was provided!
                       Available data is insufficient to compute a result.})
            (let ([id-element ((id f) (first vs))])
              (fold-method combiner-proc
                           id-element
                           vs)))
        (fold-method combiner-proc
                     base
                     vs))))

(define foldl (curry fold #:direction 'left))
(define foldr (curry fold #:direction 'right))
(define fold/steps (curry fold #:with-steps #t))
(define foldr/steps (curry fold/steps #:direction 'right))
(define foldl/steps (curry fold/steps #:direction 'left))

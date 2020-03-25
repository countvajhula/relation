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
         ID
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
          [.. (-> appendable? ... appendable?)]
          [∘ (-> appendable? ... appendable?)]
          [* (-> multipliable? ... multipliable?)]
          [/ (-> multipliable? multipliable? ... multipliable?)]
          [+ (-> addable? ... addable?)]
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
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (string-append appendable other)))
                    (define (appendable-identity appendable)
                      "")]
                   [bytes?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (bytes-append appendable other)))
                    (define (appendable-identity appendable)
                      #"")]
                   [list?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (b:append appendable other)))
                    (define (appendable-identity appendable)
                      (list))]
                   [vector?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (vector-append appendable other)))
                    (define (appendable-identity appendable)
                      #())]
                   [set?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (set-union appendable other)))
                    (define (appendable-identity appendable)
                      (set))]
                   [dict?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          ((.. make-immutable-hash
                               ->list
                               d:append)
                           appendable other)))
                    (define (appendable-identity appendable)
                      (hash))]
                   [number?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (b:+ appendable other)))
                    (define (appendable-identity appendable)
                      0)]
                   [sequence?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (d:append appendable other)))
                    (define (appendable-identity appendable)
                      (list))]
                   [procedure?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (compose appendable other)))
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
                    (define (multiply multipliable other)
                      (if (eq? other ID)
                          multipliable
                          (b:* multipliable other)))
                    (define (multipliable-identity multipliable)
                      1)
                    (define multipliable-inverse (curry b:/ 1))]))

(define-generics addable
  (add addable other)
  (addable-identity addable)
  (addable-inverse addable)
  #:fast-defaults ([number?
                    (define (add addable other)
                      (if (eq? other ID)
                          addable
                          (b:+ addable other)))
                    (define (addable-identity addable)
                      0)
                    (define addable-inverse b:-)]
                   [vector?
                    (define/generic generic-add add)
                    (define/generic generic-addable-inverse addable-inverse)
                    (define (add addable other)
                      (if (eq? other ID)
                          addable
                          ((.. ->vector
                               (curry map
                                      generic-add))
                           addable other)))
                    (define (addable-identity addable)
                      (->vector
                       (take (length addable)
                             (repeat ((id +) (first addable))))))
                    (define/thrush addable-inverse
                      (curry map
                             generic-addable-inverse)
                      ->vector)]))

(struct composition-identity ()
  #:transparent
  #:methods gen:appendable
  [(define (append appendable other)
     other)
   (define (appendable-identity appendable)
     appendable)
   (define (appendable-inverse appendable)
     appendable)]
  #:methods gen:multipliable
  [(define (multiply multipliable other)
     other)
   (define (multipliable-identity multipliable)
     multipliable)
   (define (multipliable-inverse multipliable)
     multipliable)]
  #:methods gen:addable
  [(define (add addable other)
     other)
   (define (addable-identity addable)
     addable)
   (define (addable-inverse addable)
     addable)])

(define ID (composition-identity))

(define (reify v type)
  (if (eq? v ID)
      (cond [(eq? type string?)
             ""]
            [(eq? type list?)
             (list)]
            [(eq? type vector?)
             #()]
            [(eq? type bytes?)
             #""]
            [(eq? type set?)
             (set)]
            [(eq? type dict?)
             (hash)]
            [(eq? type sequence?)
             (list)]
            [(eq? type procedure?)
             identity]
            [(eq? type number?)
             0]
            [else
             (error 'reify
                    "Unknown type ~a!"
                    type)])
      (if (type v)
          v
          (error 'reify
                 "Can't reify tangible value ~a as type ~a!"
                 v
                 type))))

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

(define (.. . vs)
  (if (empty? vs)
      ID
      (foldl append vs #:order 'bab)))

(define (* . vs)
  (if (empty? vs)
      ID
      (foldl multiply vs #:order 'bab)))

(define (/ v . remaining)
  (if (empty? remaining)
      (multipliable-inverse v)
      (apply * v (map multipliable-inverse remaining))))

(define (+ . vs)
  (if (empty? vs)
      ID
      (foldl add vs #:order 'bab)))

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
            ID
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

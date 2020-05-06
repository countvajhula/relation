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
         point-free
         relation/equivalence
         version-case)

(version-case
 [(version< (version) "7.5.0.14")
  (define string-append
    (compose string->immutable-string b:string-append))]
 [else
  (define string-append string-append-immutable)])

(provide gen:appendable
         appendable/c
         gen:multipliable
         multipliable/c
         gen:addable
         addable/c
         ID
         (contract-out
          [reify (-> any/c any/c any/c)]
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
          [sum (-> (sequenceof addable?) addable?)]
          [product (-> (sequenceof multipliable?) multipliable?)]
          [fold (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                       (unconstrained-domain-> any/c))])
                     (#:into [base any/c]
                      #:order [order (one-of/c 'abb
                                               'bab)]
                      #:direction [direction (one-of/c 'left
                                                       'right)]
                      #:with-steps? [with-steps? boolean?])
                     #:rest [seqs (listof (sequenceof any/c))]
                     [result any/c])]
          [foldl (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                        (unconstrained-domain-> any/c))])
                      (#:into [base any/c]
                       #:order [order (one-of/c 'abb
                                                'bab)]
                       #:with-steps? [with-steps? boolean?])
                      #:rest [seqs (listof (sequenceof any/c))]
                      [result any/c])]
          [foldr (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                        (unconstrained-domain-> any/c))])
                      (#:into [base any/c]
                       #:order [order (one-of/c 'abb
                                                'bab)]
                       #:with-steps? [with-steps? boolean?])
                      #:rest [seqs (listof (sequenceof any/c))]
                      [result any/c])]
          [fold/steps (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                             (unconstrained-domain-> any/c))])
                           (#:into [base any/c]
                            #:order [order (one-of/c 'abb
                                                     'bab)]
                            #:direction [direction (one-of/c 'left
                                                             'right)])
                           #:rest [seqs (listof (sequenceof any/c))]
                           [result any/c])]
          [foldl/steps (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                              (unconstrained-domain-> any/c))])
                            (#:into [base any/c]
                             #:order [order (one-of/c 'abb
                                                      'bab)])
                            #:rest [seqs (listof (sequenceof any/c))]
                            [result any/c])]
          [foldr/steps (->i ([f (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                              (unconstrained-domain-> any/c))])
                            (#:into [base any/c]
                             #:order [order (one-of/c 'abb
                                                      'bab)])
                            #:rest [seqs (listof (sequenceof any/c))]
                            [result any/c])]
          [gather (-> (sequenceof procedure?)
                      any/c
                      ...
                      any/c)]))

(define-generics appendable
  (append appendable other)
  (appendable-identity appendable)
  (appendable-inverse appendable)
  #:fallbacks [(define (appendable-inverse appendable)
                 (error 'appendable-inverse
                        "~a is not invertible under the append operation!"
                        appendable))]
  #:defaults ([procedure?
               (define (append appendable other)
                 (if (eq? other ID)
                     appendable
                     (compose appendable other)))
               (define (appendable-identity appendable)
                 identity)]
              [dict?
               (define (append appendable other)
                 (if (eq? other ID)
                     appendable
                     ((.. make-immutable-hash
                          sequence->list
                          d:append)
                      appendable other)))
               (define (appendable-identity appendable)
                 (hash))]
              [sequence?
               (define (append appendable other)
                 (if (eq? other ID)
                     appendable
                     (d:append appendable other)))
               (define (appendable-identity appendable)
                 (list))])
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
                   [number?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (b:+ appendable other)))
                    (define (appendable-identity appendable)
                      0)]
                   [stream?
                    (define (append appendable other)
                      (if (eq? other ID)
                          appendable
                          (d:append appendable other)))
                    (define (appendable-identity appendable)
                      (stream))]))

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

(define/thrush sequence->vector
  sequence->list
  list->vector
  vector->immutable-vector)

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
                          ((.. sequence->vector
                               (curry map
                                      generic-add))
                           addable other)))
                    (define (addable-identity addable)
                      (sequence->vector
                       (take (length addable)
                             (repeat ((id +) (first addable))))))
                    (define/thrush addable-inverse
                      (curry map
                             generic-addable-inverse)
                      sequence->vector)]))

(struct composition-identity ()
  #:transparent
  #:methods gen:sequence
  [(define (empty? seq)
     #t)
   (define (first seq)
     (error 'first
            "Nothing here!"))
   (define (rest seq)
     (error 'rest
            "Nothing here!"))
   (define (reverse seq)
     seq)]
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

(define (reify v example)
  (if (eq? v ID)
      ((id append) example)
      v))

(define (id operation)
  (cond [(member operation (list + add))
         addable-identity]
        [(member operation (list * multiply))
         multipliable-identity]
        [(member operation (list .. append))
         appendable-identity]
        [else (raise-argument-error 'id
                                    @~a{A canonical operation such as addition
                                        or concatenation for which the identity
                                        is inferable.}
                                    operation)]))

(define (inverse operation)
  (cond [(member operation (list + add))
         addable-inverse]
        [(member operation (list * multiply))
         multipliable-inverse]
        [(member operation (list .. append))
         appendable-inverse]
        [else (raise-argument-error 'inverse
                                    @~a{A canonical operation such as addition
                                        or concatenation for which the inverse
                                        is inferable.}
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

(define sum (curry apply +))

(define product (curry apply *))

(define (- v . remaining)
  (if (empty? remaining)
      (addable-inverse v)
      (apply + v (map addable-inverse remaining))))

(define ∘ ..)

(define (undefined? v)
  (eq? v undefined))

(define (flip$ f)
  (λ (x . args)
    (apply f (append args (list x)))))

(define (fold f
              #:into [base undefined]
              #:order [order 'abb]
              #:direction [direction 'right]
              #:with-steps? [with-steps? #f]
              . seqs)
  (let ([combiner-proc (cond [(= order 'abb)
                              (flip$ f)]
                             [(= order 'bab)
                              f]
                             [else (error 'fold
                                          "Invalid fold argument order ~a!"
                                          order)])]
        [seqs (cond [(= direction 'left) seqs]
                    [(= direction 'right) (map reverse seqs)]
                    (error 'fold
                           "Invalid fold direction ~a!"
                           direction))]
        [fold-method (if with-steps?
                         d:foldl/steps
                         d:foldl)])
    (if (undefined? base)
        (if (empty? seqs)
            ID
            (let ([vs (first seqs)])
              (if (empty? vs)
                  ID
                  (let ([id-element ((id f) (first vs))])
                    (apply fold-method
                           combiner-proc
                           id-element
                           seqs)))))
        (apply fold-method
               combiner-proc
               base
               seqs))))

(define foldl (curry fold #:direction 'left))
(define foldr (curry fold #:direction 'right))
(define fold/steps (curry fold #:with-steps? #t))
(define foldr/steps (curry fold/steps #:direction 'right))
(define foldl/steps (curry fold/steps #:direction 'left))

(define (gather fs . vs)
  (if (empty? fs)
      empty-stream
      (stream-cons (apply (first fs) vs)
                   (apply gather (rest fs) vs))))

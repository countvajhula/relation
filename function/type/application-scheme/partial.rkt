#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         contract/social
         racket/generic
         racket/hash
         (except-in racket/list
                    first
                    rest
                    empty?)
         arguments
         (prefix-in b: racket/base)
         ionic
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  collection?
                  sequence?
                  conj
                  first
                  rest
                  empty?))

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../composed.rkt"
         "../../../private/util.rkt"
         "private/util.rkt")

(provide (contract-out
          [struct partial-function
            ((f procedure?)
             (chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [make-partial-function (function/c b:procedure?
                                             arguments?
                                             symbol?
                                             partial-function?)]
          [empty-partial-function partial-function?]))

(struct partial-function function (f chirality left right kw)
  #:transparent

  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -arity arity)
   (define/generic -keywords keywords)
   (define/generic -render-function render-function)
   (define (procedure-apply this invocation-args)
     (let* ([f (partial-function-f this)]
            [updated-application (pass this invocation-args)]
            [args (flat-arguments updated-application)])
       (-procedure-apply f args)))
   (define (arity this)
     (let ([naive-arity (-arity (partial-function-f this))]
           [pos (partial-function-positional this)])
       (revise-arity naive-arity (length pos))))
   (define (keywords this)
     (let-values ([(naive-required-keywords naive-accepted-keywords)
                   (-keywords (partial-function-f this))])
       (let ([supplied-kws (hash-keys (partial-function-kw this))])
         (values (list-subtract naive-required-keywords
                                supplied-kws)
                 (and (list? naive-accepted-keywords)
                      (list-subtract naive-accepted-keywords
                                     supplied-kws))))))
   (define (render-function this)
     (let* ([f (partial-function-f this)]
            [left (partial-function-left this)]
            [right (partial-function-right this)]
            [kw (partial-function-kw this)]
            [inner-representation (-render-function f)]
            [marker-position (and (list? inner-representation)
                                  (index-of inner-representation 'λ))]
            [args (cond [(null? right)
                         (append left (list '__) (kwhash->altlist kw))]
                        [(null? left)
                         (append (list '__) right (kwhash->altlist kw))]
                        [else (append left
                                      (list '__)
                                      right
                                      (kwhash->altlist kw))])])
       (if (or (application-scheme? f)
               (not (list? inner-representation))
               (not marker-position)) ; unfamiliar function representation
           `(λ ,args ,inner-representation)
           (let-values ([(before after)
                         (split-at inner-representation
                                   (add1 marker-position))])
             `(,@before ,args ,@after)))))]

  #:methods gen:application-scheme
  [(define (pass this args)
     ;; incorporate fresh arguments into the partial application,
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([f (partial-function-f this)]
           [chirality (partial-function-chirality this)])
       (let ([left-args (if (eq? chirality 'left)
                            (append (partial-function-left this)
                                    (arguments-positional args))
                            (partial-function-left this))]
             [right-args (if (eq? chirality 'right)
                             ;; note order reversed for right args
                             (append (arguments-positional args)
                                     (partial-function-right this))
                             (partial-function-right this))])
         (partial-function f
                           chirality
                           left-args
                           right-args
                           (hash-union (partial-function-kw this)
                                       (arguments-keyword args))))))
   (define (flat-arguments this)
     (make-arguments (partial-function-positional this)
                     (partial-function-kw this)))
   (define (unwrap-application this)
     (partial-function-f this))]

  #:methods gen:collection
  [(define/generic -conj conj)
   (define (conj self elem)
     (let ([f (partial-function-f self)])
       (if (collection? f)
           (struct-copy partial-function self
                        [f (-conj f elem)])
           (struct-copy partial-function self
                        [f (-conj (-conj (function-null) f) elem)]))))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (let ([f (partial-function-f self)])
       (and (sequence? f)
            (-empty? f))))
   (define (first self)
     (let ([f (partial-function-f self)])
       (if (sequence? f)
           (struct-copy partial-function self
                        [f (-first f)])
           self)))
   (define (rest self)
     (let ([f (partial-function-f self)])
       (if (sequence? f)
           (make-partial-function (-rest f)
                                  empty-arguments
                                  (partial-function-chirality self))
           empty-partial-function)))
   (define (reverse self)
     (let ([f (partial-function-f self)])
       (if (sequence? f)
           (struct-copy partial-function self
                        [f (-reverse f)])
           self)))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (if (eq? self empty-partial-function)
         0
         (let ([f (partial-function-f self)])
           (if (sequence? f)
               (-length f)
               1))))])

(define empty-partial-function
  (partial-function (function-null)
                    'left
                    null
                    null
                    (hash)))

(define (partial-function-positional f)
  (append (partial-function-left f)
          (partial-function-right f)))

(define (make-partial-function f args chirality)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)])
    (switch (f)
      [partial-function?
       (connect [(~> partial-function-chirality
                     (eq? chirality))
                 ((esc pass) args)]
                [else (~> (gen (struct-copy partial-function
                                            f
                                            [chirality chirality]))
                          ((esc pass) args))])]
      [else (if (gen (eq? chirality 'left))
                (partial-function 'left pos null kw)
                (partial-function 'right null pos kw))])))

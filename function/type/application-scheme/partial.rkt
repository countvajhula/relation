#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/hash
         racket/list
         arguments
         (prefix-in b: racket/base)
         ionic
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  sequence?))

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
          [struct partial-atomic-function
            ((f procedure?)
             (chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [struct partial-composed-function
            ((f procedure?)
             (chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [make-partial-function (-> b:procedure?
                                     arguments?
                                     symbol?
                                     partial-function?)]))

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
                         (append left (list '_) (kwhash->altlist kw))]
                        [(null? left)
                         (append (list '_) right (kwhash->altlist kw))]
                        [else (append left
                                      (list '_)
                                      right
                                      (kwhash->altlist kw))])])
       (if (or (application-scheme? f)
               (not (list? inner-representation))
               (not marker-position)) ; unfamiliar function representation
           `(λ ,args ,inner-representation)
           (let-values ([(before after)
                         (split-at inner-representation
                                   (add1 marker-position))])
             `(,@before ,args ,@after)))))])

(struct partial-atomic-function partial-function ()
  #:transparent

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
         (partial-atomic-function f
                                  chirality
                                  left-args
                                  right-args
                                  (hash-union (partial-function-kw this)
                                              (arguments-keyword args))))))
   (define (flat-arguments this)
     (make-arguments (partial-function-positional this)
                     (partial-function-kw this)))
   (define (unwrap-application this)
     (partial-function-f this))])

(struct partial-composed-function partial-function ()
  #:transparent

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
         (partial-composed-function f
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
     (-conj (partial-function-f self) elem))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (partial-function-f self)))
   (define (first self)
     (let ([f (-first (partial-function-f self))])
       (if (sequence? f)
           (struct-copy partial-composed-function self
                        [f #:parent partial-function f])
           (partial-atomic-function f
                                    (partial-function-chirality self)
                                    (partial-function-left self)
                                    (partial-function-right self)
                                    (partial-function-kw self)))))
   (define (rest self)
     (make-partial-function (-rest (partial-function-f self))
                            empty-arguments
                            (partial-function-chirality self)))
   (define (reverse self)
     (struct-copy partial-composed-function self
                  [f #:parent partial-function (-reverse (partial-function-f self))]))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (partial-function-f self)))])

(define (partial-function-positional f)
  (append (partial-function-left f)
          (partial-function-right f)))

(define (make-partial-function f args chirality)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)]
        [f-cons (if (sequence? f)
                    partial-composed-function
                    partial-atomic-function)])
    (switch (f)
            [partial-function?
             (connect [(~> partial-function-chirality
                           (eq? chirality))
                       (call ((esc pass) args))]
                      [else (connect
                             [sequence?
                              (pass (struct-copy partial-composed-function
                                                 f
                                                 [chirality #:parent partial-function chirality])
                                    args)]
                             [else
                              (pass (struct-copy partial-atomic-function
                                                 f
                                                 [chirality #:parent partial-function chirality])
                                    args)])])]
            [else (if (eq? chirality 'left)
                      (f-cons f 'left pos null kw)
                      (f-cons f 'right null pos kw))])))

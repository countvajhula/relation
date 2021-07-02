#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/hash
         racket/list
         arguments
         (prefix-in b: racket/base)
         ionic)

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../../../private/util.rkt"
         "private/util.rkt")

(provide (contract-out
          [struct partial-function
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
           `(,inner-representation ,@args)
           (let-values ([(before after)
                         (split-at inner-representation
                                   (add1 marker-position))])
             `(,@before ,args ,@after)))))])

(define (partial-function-positional args)
  (append (partial-function-left args)
          (partial-function-right args)))

(define (make-partial-function f args chirality)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)])
    (if (eq? 'left chirality)
        (partial-function f 'left pos null kw)
        (partial-function f 'right null pos kw))))

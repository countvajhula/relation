#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/generic
         racket/hash
         racket/set
         arguments
         relation/logic
         (prefix-in b: racket/base)
         (only-in relation/equivalence
                  in?)
         ionic)

(require "interface.rkt"
         "../interface.rkt"
         "../base.rkt"
         "../util.rkt"
         "../../../private/util.rkt")

(provide (contract-out
          [struct partial-arguments
            ((f procedure?)
             (chirality symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [make-partial-arguments (-> b:procedure?
                                      symbol?
                                      arguments?
                                      partial-arguments?)]))

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (call (~>> (map ~min-arity-value) (apply min)))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (~min-arity f)
  (~min-arity-value (arity f)))

(struct partial-arguments function (f chirality left right kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args)
     ;; incorporate fresh arguments into the partial application,
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([f (partial-arguments-f this)]
           [chirality (partial-arguments-chirality this)])
       (let ([left-args (if (eq? chirality 'left)
                            (append (partial-arguments-left this)
                                    (arguments-positional args))
                            (partial-arguments-left this))]
             [right-args (if (eq? chirality 'right)
                             ;; note order reversed for right args
                             (append (arguments-positional args)
                                     (partial-arguments-right this))
                             (partial-arguments-right this))])
         (partial-arguments f
                            chirality
                            left-args
                            right-args
                            (hash-union (partial-arguments-kw this)
                                        (arguments-keyword args))))))
   (define (flat-arguments this)
     (make-arguments (partial-arguments-positional this)
                     (partial-arguments-kw this)))]

  #:methods gen:procedure
  [(define/generic -procedure-apply procedure-apply)
   (define/generic -arity arity)
   (define/generic -keywords keywords)
   (define (procedure-apply this invocation-args)
     (let* ([f (partial-arguments-f this)]
            [updated-application (pass this invocation-args)]
            [args (flat-arguments updated-application)])
       (-procedure-apply f args)))
   (define (arity this)
     ;; TODO: subtract args already supplied
     (-arity (partial-arguments-f this)))
   (define (keywords this)
     ;; TODO: subtract args already supplied
     (-keywords (partial-arguments-f this)))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([left (partial-arguments-left self)]
           [right (partial-arguments-right self)]
           [kw (partial-arguments-kw self)])
       (cond [(null? right)
              (recur (append left (list '_)) port)]
             [(null? left)
              (recur (append (list '_) right) port)]
             [else (recur (append left
                                  (list '_)
                                  right
                                  (kwhash->altlist kw))
                          port)])))])

(define (partial-arguments-positional args)
  (append (partial-arguments-left args)
          (partial-arguments-right args)))

(define (make-partial-arguments f chirality args)
  (let ([pos (arguments-positional args)]
        [kw (arguments-keyword args)])
    (if (eq? 'left chirality)
        (partial-arguments f 'left pos null kw)
        (partial-arguments f 'right null pos kw))))

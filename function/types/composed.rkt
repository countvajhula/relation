#lang racket/base

(require racket/function
         racket/contract/base
         racket/match
         racket/generic
         racket/stream
         (except-in racket/list
                    empty?
                    first
                    rest)
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  empty?
                  first
                  rest
                  nth
                  sequence?
                  sequence->list
                  reverse
                  repeat)
         arguments
         ionic)

(require "procedure.rkt"
         "application-scheme.rkt"
         "base.rkt"
         "util.rkt")

(provide (contract-out
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct base-composed-function ((applier application-scheme?)
                                          (composer monoid?))]
          [struct composed-function ((applier application-scheme?)
                                     (composer monoid?)
                                     (components list?))]
          [make-composed-function (->* ()
                                       (#:compose-with monoid?
                                        #:apply-with application-scheme?)
                                       #:rest (listof procedure?)
                                       composed-function?)]
          [apply/steps (unconstrained-domain-> sequence?)])
         usual-composition
         conjoin-composition
         disjoin-composition)

(struct monoid (f id)
  #:transparent
  #:property prop:procedure
  (λ (self . vs)
    (foldl (flip (monoid-f self))
           (monoid-id self)
           vs)))

(define usual-composition (monoid compose values))
(define conjoin-composition (monoid conjoin true.))
(define disjoin-composition (monoid disjoin false.))

(struct base-composed-function function (composer)
  #:transparent)

(struct composed-function base-composed-function (components)
  #:transparent

  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define/generic -procedure-apply procedure-apply)
   (define (keywords self)
     (let ([leading-function (switch ((composed-function-components self))
                                     [null? (monoid-id (base-composed-function-composer self))]
                                     [else (call last)])])
       (-keywords leading-function)))
   (define (arity self)
     (let ([leading-function (switch ((composed-function-components self))
                                     [null? (monoid-id (base-composed-function-composer self))]
                                     [else (call last)])])
       (-arity leading-function)))
   (define (procedure-apply self args)
     (let ([components (composed-function-components self)]
           [composer (base-composed-function-composer self)])
       (-procedure-apply (apply composer components)
                         args)))
   (define (pass-args self args chirality)
     (struct-copy composed-function self
                  [applier #:parent function
                           (pass (function-applier self)
                                 args
                                 chirality)]))]

  #:methods gen:collection
  [(define (conj self elem)
     (struct-copy composed-function self
                  [components (cons elem (composed-function-components self))]))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (composed-function-components self)))
   (define (first self)
     (-first (composed-function-components self)))
   (define (rest self)
     (composed-function (function-applier self)
                        (base-composed-function-composer self)
                        (-rest (composed-function-components self))))
   (define (reverse self)
     (composed-function (function-applier self)
                        (base-composed-function-composer self)
                        (-reverse (composed-function-components self))))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (composed-function-components self)))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([applier (function-applier self)]
            [composer (base-composed-function-composer self)]
            [components (composed-function-components self)]
            [representation
             (list 'λ
                   applier
                   (list* (match composer
                            [(== usual-composition) '..]
                            [(== conjoin-composition) '&&]
                            [(== disjoin-composition) '||]
                            [_ '??])
                          components))])
       (recur representation port)))])

(define (make-composed-function #:compose-with [composer usual-composition]
                                #:apply-with [applier empty-left-curried-arguments]
                                . fs)
  (composed-function applier
                     composer
                     fs))

(define/arguments (apply/steps args)
  (let ([f (first (arguments-positional args))]
        ;; list* here to support rolling args into the tail in the syntax
        ;; (apply/steps f arg1 arg2 ... rest-args)
        [args (make-arguments (apply list* (rest (arguments-positional args)))
                              (arguments-keyword args))])
    (if (empty? f)
        (stream (apply/arguments f args))
        (let* ([remf (reverse f)]
               [v (apply/arguments (first remf)
                                   args)])
          (stream-cons v
                       (let loop ([remf (rest remf)]
                                  [v v])
                         (if (empty? remf)
                             empty-stream
                             (let ([v ((first remf) v)])
                               (stream-cons v
                                            (loop (rest remf)
                                                  v))))))))))

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

(require "interface.rkt"
         "base.rkt"
         "util.rkt")

(provide (contract-out
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct base-composed-function ((composer monoid?))]
          [struct composed-function ((composer monoid?)
                                     (components list?))]
          [make-composed-function (->* ()
                                       (#:thread? boolean?
                                        #:compose-with monoid?)
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
    (foldl (monoid-f self)
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
   (define (render-function self)
     (let ([components (composed-function-components self)]
           [composer (base-composed-function-composer self)])
       (list 'λ
             (list* (match composer
                      [(== usual-composition) '..]
                      [(== conjoin-composition) '&&]
                      [(== disjoin-composition) '||]
                      [_ '??])
                    components))))]

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
     (composed-function (base-composed-function-composer self)
                        (-rest (composed-function-components self))))
   (define (reverse self)
     (composed-function (base-composed-function-composer self)
                        (-reverse (composed-function-components self))))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (composed-function-components self)))])

(define (make-composed-function #:thread? [thread? #f]
         #:compose-with [composer usual-composition]
                                . fs)
  (composed-function composer
                     (if thread?
                         fs
                         (reverse fs))))

(define/arguments (apply/steps args)
  (let ([f (first (arguments-positional args))]
        ;; list* here to support rolling args into the tail in the syntax
        ;; (apply/steps f arg1 arg2 ... rest-args)
        [args (make-arguments (apply list* (rest (arguments-positional args)))
                              (arguments-keyword args))])
    (if (empty? f)
        (stream (apply/arguments f args))
        (let ([v (apply/arguments (first f) args)])
          (stream-cons v
                       (let loop ([remf (rest f)]
                                  [v v])
                         (if (empty? remf)
                             empty-stream
                             (let ([v ((first remf) v)])
                               (stream-cons v
                                            (loop (rest remf)
                                                  v))))))))))

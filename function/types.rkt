#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/match
         racket/generic
         racket/set
         racket/lazy-require
         (except-in racket/list
                    empty?
                    first
                    rest)
         arguments
         syntax/parse/define
         (prefix-in b: racket/base)
         (only-in data/collection
                  gen:collection
                  gen:sequence
                  gen:countable
                  empty?
                  first
                  rest
                  nth
                  sequence->list
                  reverse
                  repeat)
         mischief/shorthand
         contract/social
         relation/logic
         (only-in relation/equivalence
                  in?)
         syntax/on)

(require relation/function/application-scheme
         relation/function/core
         "../private/util.rkt")

;; so the power-function type can use the `power` utility
(lazy-require [relation/composition (power)])

(provide lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         gen:procedure
         procedure/c
         usual-composition
         conjoin-composition
         disjoin-composition
         (contract-out
          [struct function ((applier application-scheme?)
                            (chirality symbol?))]
          [struct atomic-function ((applier application-scheme?)
                                   (chirality symbol?)
                                   (f procedure?))]
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct composed-function ((applier application-scheme?)
                                     (chirality symbol?)
                                     (components list?)
                                     (composer monoid?))]
          [struct power-function ((applier application-scheme?)
                                  (chirality symbol?)
                                  (f procedure?)
                                  (n number?))]
          [make-function (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?
                               #:curry-on symbol?)
                              #:rest (listof procedure?)
                              function?)]
          [make-composed-function (->* ()
                                       (#:compose-with monoid?
                                        #:apply-with application-scheme?
                                        #:curry-on symbol?)
                                       #:rest (listof procedure?)
                                       composed-function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:apply-with application-scheme?
                                         #:curry-on symbol?)
                                        #:rest (listof procedure?)
                                        composed-function?)]
          [make-power-function (->* (procedure? number?)
                                    (#:apply-with application-scheme?
                                     #:curry-on symbol?)
                                    power-function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:apply-with application-scheme?
                   #:curry-on symbol?)
                  #:rest (listof procedure?)
                  function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:apply-with application-scheme?
                    #:curry-on symbol?)
                   #:rest (listof procedure?)
                   composed-function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?
                               #:curry-on symbol?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-flat-arguments (function/c function? arguments?)]))

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (apply min (map ~min-arity-value))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (~min-arity f)
  (~min-arity-value (arity f)))

(struct monoid (f id)
  #:transparent
  #:property prop:procedure
  (λ (self . vs)
    (foldl (flip (monoid-f self))
           (monoid-id self)
           vs)))

(define usual-composition (monoid b:compose values))
(define conjoin-composition (monoid f:conjoin true.))
(define disjoin-composition (monoid f:disjoin false.))

(define (eval-if-saturated f applier)
  ;; attempt to eval the function. If it fails, return a new
  ;; function with a modified applier
  (let* ([args (flat-arguments applier)]
         [pos-args (arguments-positional args)]
         [kw-args (arguments-keyword args)])
    (with-handlers ([recoverable-apply-error?
                     ;; if it gets to the eval stage, the application scheme
                     ;; at this level has already signed off on it, but a nested
                     ;; application scheme is not yet fulfilled. We consult
                     ;; the application scheme on what to do here
                     (λ (exn)
                       (update-application f (handle-failure applier exn)))]
                    [exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity f))
                           (raise exn)
                           (update-application f (handle-failure applier exn))))]
                    [exn:fail:contract?
                     ;; presence of a keyword argument results in a premature
                     ;; contract failure that's not the arity error, even though
                     ;; that's probably what it should be since providing additional
                     ;; positional arguments results in expected behavior
                     ;; additionally, also handle invalid keyword arg here
                     (λ (exn)
                       (let-values ([(req-kw opt-kw)
                                     (keywords f)])
                         (if (or (empty? kw-args)
                                 ;; the arity error is masked in the presence of keyword
                                 ;; args so we check for it again here
                                 (> (length pos-args)
                                    (~min-arity f))
                                 ;; any unexpected keywords?
                                 (any?
                                  (map (!! (in? (append req-kw opt-kw)))
                                       (hash-keys kw-args)))
                                 ;; all required arguments received?
                                 (and (subset? req-kw (hash-keys kw-args))
                                      (>= (length pos-args)
                                          (~min-arity f))))
                             (raise exn)
                             (update-application f (handle-failure applier exn)))))])
      (procedure-apply f args))))

(define-generics procedure
  (keywords procedure)
  (arity procedure)
  (procedure-apply procedure args)
  ;; (pass-args procedure args)
  ;; TODO: can this and the application scheme's handle-failure be merged?
  ;; this consideration could shed light on the interplay between the two
  (update-application procedure applier)
  #:defaults
  ([b:procedure?
    (define keywords procedure-keywords)
    (define arity procedure-arity)
    ;; (define pass-args (arg 0))
    (define procedure-apply apply/arguments)
    (define update-application (arg 0))]))

(struct function (applier
                  chirality)
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))]
          [applier (function-applier self)]
          [updated-applier (pass applier
                                 args
                                 (function-chirality self))])
     (eval-if-saturated self updated-applier))))

(struct atomic-function function (f)
  #:transparent

  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define/generic -procedure-apply procedure-apply)
   (define/generic -update-application update-application)
   (define (keywords self)
     (-keywords (atomic-function-f self)))
   (define (arity self)
     (-arity (atomic-function-f self)))
   (define (procedure-apply self args)
     (apply/arguments (atomic-function-f self) args))
   (define (update-application self applier)
     (struct-copy atomic-function self
                  [applier #:parent function
                           applier]))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([applier (function-applier self)]
            [f (atomic-function-f self)]
            [representation
             (list 'λ
                   applier
                   f)])
       (recur representation port)))])

(struct composed-function function (components
                                    composer)
  #:transparent

  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define/generic -procedure-apply procedure-apply)
   (define/generic -update-application update-application)
   (define (keywords self)
     (let ([leading-function (switch ((composed-function-components self))
                                     [null? (monoid-id (composed-function-composer self))]
                                     [else (call last)])])
       (-keywords leading-function)))
   (define (arity self)
     (let ([leading-function (switch ((composed-function-components self))
                                     [null? (monoid-id (composed-function-composer self))]
                                     [else (call last)])])
       (-arity leading-function)))
   (define (procedure-apply self args)
     (let ([components (composed-function-components self)]
           [composer (composed-function-composer self)])
       (-procedure-apply (apply composer components)
                         args)))
   (define (update-application self applier)
     (struct-copy composed-function self
                  [applier #:parent function
                           applier]))]

  #:methods gen:collection
  [(define (conj self elem)
     (function-cons elem self))]

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
                        (function-chirality self)
                        (-rest (composed-function-components self))
                        (composed-function-composer self)))
   (define (reverse self)
     (composed-function (function-applier self)
                        (function-chirality self)
                        (-reverse (composed-function-components self))
                        (composed-function-composer self)))]

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
            [components (composed-function-components self)]
            [composer (composed-function-composer self)]
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

(define (make-atomic-function g
                              #:apply-with [applier empty-curried-arguments]
                              #:curry-on [chirality 'left])
  (atomic-function applier
                   chirality
                   g))

(define (make-composed-function #:compose-with [composer usual-composition]
                                #:apply-with [applier empty-curried-arguments]
                                #:curry-on [chirality 'left]
                                . fs)
  (composed-function applier
                     chirality
                     fs
                     composer))

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       #:curry-on [chirality 'left]
                       . fs)
  (if (singleton? fs)
      (atomic-function applier
                       chirality
                       (first fs))
      ;; TODO: use compose interface
      (composed-function applier
                         chirality
                         fs
                         composer)))


(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-curried-arguments]
                                 #:curry-on [chirality 'left]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         #:curry-on chirality
         (reverse fs)))

(define f> make-threading-function)

(define (make-power-function g n
                             #:apply-with [applier empty-curried-arguments]
                             #:curry-on [chirality 'left])
  (power-function applier
                  chirality
                  g
                  n))

(struct power-function function (f n)
  #:transparent
  #:methods gen:procedure
  [(define/generic -keywords keywords)
   (define/generic -arity arity)
   (define/generic -procedure-apply procedure-apply)
   (define/generic -update-application update-application)
   (define (keywords self)
     (-keywords (power-function-f self)))
   (define (arity self)
     (-arity (power-function-f self)))
   (define (procedure-apply self args)
     (-procedure-apply (power (power-function-f self) (power-function-n self)) args))
   (define (update-application self applier)
     (struct-copy power-function self
                  [applier #:parent function
                           applier]))])

(define-syntax-rule (lambda/function kw-formals body ...)
  (f (lambda kw-formals body ...)))

(define-alias lambda/f lambda/function)

(define-alias λ/f lambda/function)

(define-simple-macro (define/function (id:id kw-formals ... . rest-args)
                       body ...)
  (define id
    (lambda/function (kw-formals ... . rest-args)
                     body ...)))

(define-alias define/f define/function)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       #:curry-on [chirality 'left])
  (make-function #:compose-with composer
                 #:apply-with applier
                 #:curry-on chirality))

(define (function-cons proc f)
  (switch (f)
    [atomic-function?
     (composed-function (function-applier f)
                        (function-chirality f)
                        (cons proc (list (atomic-function-f f)))
                        usual-composition)] ;
    [composed-function?
     (struct-copy composed-function f
                  [components (cons proc (composed-function-components f))])]
    [else (compose proc f)]))

(define (function-flat-arguments f)
  (flat-arguments (function-applier f)))

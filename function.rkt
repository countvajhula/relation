#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/match
         racket/generic
         racket/stream
         racket/hash
         racket/set
         racket/format
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
         (only-in data/functor
                  (map f:map))
         mischief/shorthand
         contract/social
         relation/logic
         (except-in data/maybe maybe/c)
         typed-stack
         (only-in relation/equivalence
                  in?)
         (for-syntax racket/base
                     syntax/parse/define)
         (only-in kw-utils/kw-hash
                  apply/kw-hash)
         syntax/on)

(require "private/util.rkt")

;; so the power-function type can use the `power` utility
(lazy-require [relation/composition (power)])

(provide lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         lambda.
         λ.
         app
         gen:application-scheme
         application-scheme/c
         call
         (contract-out
          [application-scheme? (predicate/c)]
          [unthunk (binary-variadic-function/c procedure? any/c procedure?)]
          [if-f (-> (unconstrained-domain-> boolean?)
                    procedure?
                    procedure?
                    procedure?)]
          [true. (unconstrained-domain-> boolean?)]
          [false. (unconstrained-domain-> boolean?)]
          [arg (function/c natural-number/c procedure?)]
          [flip functional/c]
          [flip$ functional/c]
          [flip* functional/c]
          [lift functional/c]
          [pack (binary-variadic-function/c procedure? any/c sequence?)]
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct function ((applier application-scheme?)
                            (chirality symbol?)
                            (components list?)
                            (composer monoid?))]
          [struct curried-arguments
            ((left list?)
             (right list?)
             (kw hash?))]
          [struct template-arguments
            ((pos list?)
             (kw hash?))]
          [empty-curried-arguments curried-arguments?]
          [make-function (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              #:rest (listof procedure?)
                              function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:apply-with application-scheme?)
                                        #:rest (listof procedure?)
                                        function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:apply-with application-scheme?)
                  #:rest (listof procedure?)
                  function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:apply-with application-scheme?)
                   #:rest (listof procedure?)
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-flat-arguments (function/c function? arguments?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (variadic-function/c procedure? function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [partial (unconstrained-domain-> function?)]
          [partial/template (unconstrained-domain-> function?)]
          [uncurry (functional/c)]
          [conjoin (variadic-function/c procedure? function?)]
          [&& (variadic-function/c procedure? function?)]
          [disjoin (variadic-function/c procedure? function?)]
          [|| (variadic-function/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]))

;; from mischief/function - reproviding it via require+provide runs aground
;; of some "name is protected" error while building docs, not sure why;
;; so including the implementation directly here for now
(define call
  (make-keyword-procedure
   (lambda (ks vs f . xs)
     (keyword-apply f ks vs xs))))

(define (unthunk f . args)
  (f:thunk*
   (apply f args)))

(define (if-f pred f g)
  (λ args
    (if (apply pred args)
        (apply f args)
        (apply g args))))

(define true.
  (procedure-rename (f:const #t)
                    'true.))

(define false.
  (procedure-rename (f:const #f)
                    'false.))

(define (arg n)
  (λ args
    (nth args n)))

(define (flip f)
  (λ (x y . args)
    (apply f y x args)))

(define (flip$ f)
  (λ (x . args)
    (apply f (append args (list x)))))

(define (flip* f)
  (λ args
    (apply f (reverse args))))

(define (lift f)
  (curry f:map f))

(define (pack f . args)
  (sequence->list (map f args)))

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (apply min (lift ~min-arity-value))]
  [else (raise-argument-error 'min-arity
                              "normalized-arity?"
                              arity)])

(define (~min-arity f)
  (~min-arity-value (funxion-arity f)))

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

;; TODO: ideally add tests for method implementations in each
;; application-scheme in a test submodule
(define-generics application-scheme
  ;; pass accepts an arguments structure representing args provided in a
  ;; single invocation, and returns an updated application-scheme instance
  ;; if the arguments are acceptable, otherwise, it raises an error that
  ;; may be either recoverable or non-recoverable. Recoverable errors
  ;; (for instance, insufficient arguments; on the other hand, excess
  ;; or invalid arguments are non-recoverable) could be handled in an
  ;; outer application scheme while non-recoverable (any other) errors
  ;; would simply be raised to the caller
  (pass application-scheme args chirality)

  ;; flat-arguments compiles all previously supplied arguments
  ;; into a "flat" arguments structure that represents the
  ;; arguments for the invocation of the underlying function
  (flat-arguments application-scheme)

  ;; handle-failure is expected to either
  ;; 1. return a modified application scheme instance, OR
  ;; 2. raise an exception
  (handle-failure application-scheme exception)

  #:defaults
  ([arguments? (define (pass this args chirality)
                 (if (eq? chirality 'left)
                     (arguments-merge this args)
                     (arguments-merge args this)))
               (define (flat-arguments this)
                 this)
               (define (handle-failure this exception)
                 (raise exception))]))

(struct curried-arguments (left right kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args chirality)
     ;; incorporate fresh arguments into the partial application,
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([left-args (if (eq? chirality 'left)
                          (append (curried-arguments-left this)
                                  (arguments-positional args))
                          (curried-arguments-left this))]
           [right-args (if (eq? chirality 'right)
                           ;; note order reversed for right args
                           (append (arguments-positional args)
                                   (curried-arguments-right this))
                           (curried-arguments-right this))])
       (curried-arguments left-args
                          right-args
                          (hash-union (curried-arguments-kw this)
                                      (arguments-keyword args)))))
   (define (flat-arguments this)
     (make-arguments (curried-arguments-positional this)
                     (curried-arguments-kw this)))
   (define (handle-failure this exception)
     this)]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([left (curried-arguments-left self)]
           [right (curried-arguments-right self)]
           [kw (curried-arguments-kw self)])
       (cond [(null? right)
              (recur (append left (list '_)) port)]
             [(null? left)
              (recur (append (list '_) right) port)]
             [else (recur (append left
                                  (list '_)
                                  right
                                  (kwhash->altlist kw))
                          port)])))])

(define empty-curried-arguments
  (curried-arguments null null (hash)))

(define (curried-arguments-positional args)
  (append (curried-arguments-left args)
          (curried-arguments-right args)))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(struct recoverable-apply-error exn:fail:contract ())

(struct template-arguments (pos kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (pass this args chirality)
     (define n-expected-args
       (length (filter nothing? (template-arguments-pos this))))
     (define arg-stack (apply make-stack (arguments-positional args)))
     (define filled-in-pos-template
       (for/list ([arg (template-arguments-pos this)])
         (if (just? arg)
             arg
             (if (stack-empty? arg-stack)
                 (raise (recoverable-apply-error (~a "Not enough arguments, expected: "
                                                     n-expected-args)
                                                 (current-continuation-marks)))
                 (just (pop! arg-stack))))))
     (unless (stack-empty? arg-stack)
       ;; too many args provided
       ;; TODO: better error reporting
       (raise-arity-error 'pass
                          n-expected-args
                          (stack->list arg-stack)))
     (define filled-in-kw-template
       (for/hash ([k (hash-keys (template-arguments-kw this))])
         (let ([v (hash-ref (template-arguments-kw this) k)])
           (values k (from-just (hash-ref (arguments-keyword args) k nothing)
                                v)))))
     ;; invocation kwargs should be present in blank template keys
     (define expected-keys
       (for/list ([k (hash-keys (template-arguments-kw this))]
                  #:when (nothing? (hash-ref (template-arguments-kw this) k)))
         k))
     (hash-for-each
      (arguments-keyword args)
      (λ (k v)
        (unless (member k expected-keys)
          (raise-arguments-error
           'pass
           "Unexpected keyword argument provided to template!"
           "keyword" k))))
     ;; all blank template keys should be present in invocation kwargs
     (hash-for-each
      filled-in-kw-template
      (λ (k v)
        (when (nothing? v)
          (raise (recoverable-apply-error (~a "Missing keyword argument in template!\n"
                                              "keyword: " k)
                                          (current-continuation-marks))))))
     (template-arguments filled-in-pos-template
                         filled-in-kw-template))

   (define (flat-arguments this)
     (make-arguments (filter-just (template-arguments-pos this))
                     (template-arguments-kw this)))

   (define (handle-failure this exception)
     (raise exception))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([pos (template-arguments-pos self)]
           [kw (template-arguments-kw self)])
       (recur (append (map (f:curry from-just '_) pos)
                      (join-list (hash-map kw
                                           (λ (k v)
                                             (list k (from-just '_ v))))))
              port)))])

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
                       (struct-copy function f
                                    [applier #:parent base-function
                                             (handle-failure applier exn)]))]
                    [exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity f))
                           (raise exn)
                           (struct-copy function f
                                        [applier #:parent base-function
                                                 (handle-failure applier exn)])))]
                    [exn:fail:contract?
                     ;; presence of a keyword argument results in a premature
                     ;; contract failure that's not the arity error, even though
                     ;; that's probably what it should be since providing additional
                     ;; positional arguments results in expected behavior
                     ;; additionally, also handle invalid keyword arg here
                     (λ (exn)
                       (let-values ([(req-kw opt-kw)
                                     (funxion-keywords f)])
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
                             (struct-copy function f
                                          [applier #:parent base-function
                                                   (handle-failure applier exn)]))))])
      (funxion-apply f args))))

(define-generics funxion
  (funxion-keywords funxion)
  (funxion-arity funxion)
  (funxion-apply funxion args)
  #:defaults
  ([procedure?
    (define funxion-keywords procedure-keywords)
    (define funxion-arity procedure-arity)
    (define funxion-apply apply/arguments)]))

(struct base-function (applier
                       chirality)
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))]
          [applier (base-function-applier self)]
          [updated-applier (pass applier
                                 args
                                 (base-function-chirality self))])
     (eval-if-saturated self updated-applier))))

(struct function base-function (components
                                composer)
  #:transparent

  #:methods gen:funxion
  [(define/generic -funxion-keywords funxion-keywords)
   (define/generic -funxion-arity funxion-arity)
   (define/generic -funxion-apply funxion-apply)
   (define (funxion-keywords self)
     (let ([leading-function (switch ((function-components self))
                                     [null? (monoid-id (function-composer self))]
                                     [else (call last)])])
       (-funxion-keywords leading-function)))
   (define (funxion-arity self)
     (let ([leading-function (switch ((function-components self))
                                     [null? (monoid-id (function-composer self))]
                                     [else (call last)])])
       (-funxion-arity leading-function)))
   (define (funxion-apply self args)
     (let ([components (function-components self)]
           [composer (function-composer self)])
       (-funxion-apply (apply composer components)
                       args)))]

  #:methods gen:collection
  [(define (conj self elem)
     (function-cons elem self))]

  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (function-components self)))
   (define (first self)
     (-first (function-components self)))
   (define (rest self)
     (function (base-function-applier self)
               (base-function-chirality self)
               (-rest (function-components self))
               (function-composer self)))
   (define (reverse self)
     (function (base-function-applier self)
               (base-function-chirality self)
               (-reverse (function-components self))
               (function-composer self)))]

  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (function-components self)))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let* ([applier (base-function-applier self)]
            [components (function-components self)]
            [composer (function-composer self)]
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

(define (make-function #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments]
                       #:curry-on [chirality 'left]
                       . fs)
  (function applier
            chirality
            fs
            composer))

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

(struct power-function base-function (f n)
  #:transparent
  #:methods gen:funxion
  [(define/generic -funxion-keywords funxion-keywords)
   (define/generic -funxion-arity funxion-arity)
   (define/generic -funxion-apply funxion-apply)
   (define (funxion-keywords self)
     (-funxion-keywords (power-function-f self)))
   (define (funxion-arity self)
     (-funxion-arity (power-function-f self)))
   (define (funxion-apply self args)
     (-funxion-apply (power (power-function-f self) (power-function-n)) args))])

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

(define-simple-macro (lambda. v ...
                              (~or* (~datum ->) (~datum →))
                              body:expr ...)
  (lambda/f (v ...)
    body ...))

(define-alias λ. lambda.)

(define (function-null #:compose-with [composer usual-composition]
                       #:apply-with [applier empty-curried-arguments])
  (make-function #:compose-with composer
                 #:apply-with applier))

(define (function-cons proc f)
  (function (base-function-applier f)
            (base-function-chirality f)
            (cons proc (function-components f))
            (function-composer f)))

(define (function-flat-arguments f)
  (flat-arguments (base-function-applier f)))

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
(define (compose-powers g h)
  ;; either or both could be function powers. in that case, the powers
  ;; need to be added; otherwise just incremented - actually just
  ;; map a priori to function-powers that would bbe set to 1, like a
  ;; free functor, and then compose them as function powers
  (let ([n (power-function-n g)]
        [m (power-function-n h)])
    (struct-copy power-function h
                 [n (+ (power-function-n g)
                       (power-function-n h))])))

(define-switch (underlying-function v)
  [power-function? (call power-function-f)]
  [else v])

(define-predicate (~compatible? g h)
  (or (any (not function?))
      (with-key base-function-applier eq?)
      (with-key function-composer eq?)))

(define-switch (function->power-function g)
  [power-function? g]
  [else (power-function g 1)])

(define-switch (function-compose g h)
  ;; this composes functions "naively," wrapping the components with a
  ;; new function in all cases but those where the applier and composer
  ;; of the component functions are eq?
  ;; It could be improved to define the nature of composition for homogeneous
  ;; and heterogeneous composition and application schemes formally
  [(all power-function?)
   (call compose-powers)]
  [(or eq?
       equal?
       (and ~compatible?
            (with-key underlying-function
              equal?)))
   (call (.. compose-powers
             (% function->power-function)))]
  [(and (all function?)
        ~compatible?) ; compose at same level
   (struct-copy function h
                [components (append (function-components g)
                                    (function-components h))])]
  [else (call f)]) ; naive composition

;; rename function -> composed-function
;; generic interface "function"
;; .. just composes whatever is there - whether primitive or rich function
;; maybe we need to decouple the application scheme from composed-function
;; so it's part of the generic interface itself somehow -- that's the rich type
;; the composed function implements this and inherits from it, and provides
;; additional goodies for composition
;; power does the same for powers
;; ... any others?
;; maybe function is just a struct, and composed inherits from it? root contains
;; applier, e.g.
;; 

(define (compose . gs)
  (let ([lifted-gs (reverse (map (if-f function? values f) gs))])
    (if (empty? lifted-gs)
        (function-null)
        (if (empty? (rest lifted-gs))
            (first lifted-gs)
            (foldl function-compose (first lifted-gs) (rest lifted-gs))))))

(define (~curry chirality func invocation-args)
  (if (and (function? func)
           (curried-arguments? (base-function-applier func)))
      ;; application scheme is compatible so just apply the
      ;; new args to the existing scheme
      (function (pass (base-function-applier func)
                      invocation-args
                      chirality)
                chirality
                (function-components func)
                (function-composer func))
      ;; wrap the existing function with one that will be curried
      (f func
         #:curry-on chirality
         #:apply-with (pass empty-curried-arguments
                            invocation-args
                            chirality))))

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (~curry 'left f invocation-args)))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (~curry 'right f invocation-args)))

(define/arguments (partial args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (f func #:apply-with invocation-args)))

(define/arguments (partial/template args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)])
    (f func #:apply-with (template-arguments pos kw))))

(define-syntax-parser app-positional-parser
  [(_) #'null]
  [(_ k:keyword _ vs ...)
   #'(app-positional-parser vs ...)]
  [(_ (~datum _) vs ...)
   #'(append (list nothing)
             (app-positional-parser vs ...))]
  [(_ v vs ...)
   #'(append (list (just v))
             (app-positional-parser vs ...))])

(define-syntax-parser app-keyword-parser
  [(_) #'(hash)]
  [(_ k:keyword (~datum _) vs ...)
   #'(hash-union (hash 'k nothing)
                 (app-keyword-parser vs ...))]
  [(_ k:keyword v:expr vs ...)
   #'(hash-union (hash 'k (just v))
                 (app-keyword-parser vs ...))]
  [(_ v vs ...)
   #'(app-keyword-parser vs ...)])

(define-syntax-parser app
  [(_ func:expr vs ...)
   #'(apply/kw-hash partial/template
                    (app-keyword-parser vs ...)
                    func
                    (app-positional-parser vs ...))])

(define (uncurry f)
  (λ/f args
    (let loop ([rem-args (reverse args)])
      (match rem-args
        ['() (f)]
        [(list v) (f v)]
        [(list v vs ...) ((loop vs) v)]))))

(define (conjoin . fs)
  (apply f
         #:compose-with conjoin-composition
         fs))

(define (disjoin . fs)
  (apply f
         #:compose-with disjoin-composition
         fs))

(define (negate g)
  (f not g))

(define && conjoin)
(define || disjoin)
(define !! negate)

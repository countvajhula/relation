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
         (except-in data/maybe maybe/c)
         typed-stack
         (for-syntax racket/base
                     syntax/parse/define)
         (only-in kw-utils/kw-hash
                  apply/kw-hash)
         syntax/on)

(require relation/logic
         (only-in relation/equivalence
                  in?)
         relation/function/application-scheme
         "private/util.rkt")

;; so the power-function type can use the `power` utility
(lazy-require [relation/composition (power)])

(provide (all-from-out
          relation/function/application-scheme)
         lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         lambda.
         λ.
         call
         app
         (contract-out
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
          [make-function (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              #:rest (listof procedure?)
                              function?)]
          [make-composed-function (->* ()
                                       (#:compose-with monoid?
                                        #:apply-with application-scheme?)
                                       #:rest (listof procedure?)
                                       composed-function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:apply-with application-scheme?)
                                        #:rest (listof procedure?)
                                        composed-function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:apply-with application-scheme?)
                  #:rest (listof procedure?)
                  function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:apply-with application-scheme?)
                   #:rest (listof procedure?)
                   composed-function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:apply-with application-scheme?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-flat-arguments (function/c function? arguments?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (variadic-function/c procedure? function?)]
          [uncurry (functional/c)]
          [conjoin (variadic-function/c procedure? function?)]
          [&& (variadic-function/c procedure? function?)]
          [disjoin (variadic-function/c procedure? function?)]
          [|| (variadic-function/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [partial (unconstrained-domain-> function?)]
          [partial/template (unconstrained-domain-> function?)]))

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
  (b:map f args))

(define-switch (~min-arity-value arity)
  [number? arity]
  [arity-at-least? (call arity-at-least-value)]
  [list? (apply min (lift ~min-arity-value))]
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
    (struct-copy power-function g
                 [n (+ (power-function-n g)
                       (power-function-n h))])))

(define-switch (underlying-function v)
  [power-function? (call power-function-f)]
  [else v])

(define-predicate (~compatible-compositions? g h)
  (and (with-key function-applier eq?)
       (with-key composed-function-composer eq?)))

(define-switch (->power-function g)
  [power-function? g]
  [else (make-power-function g 1)])

(define-switch (function-compose g h)
  ;; this composes functions "naively," wrapping the components with a
  ;; new function in all cases but those where the applier and composer
  ;; of the component functions are eq?
  ;; It could be improved to define the nature of composition for homogeneous
  ;; and heterogeneous composition and application schemes formally
  [(and (all power-function?)
        (with-key underlying-function eq?))
   (call compose-powers)]
  [(and (any power-function?)
        (with-key underlying-function eq?))
   (call (.. compose-powers (% ->power-function)))]
  [(or eq?
       equal?
       (and (all composed-function?)
            ~compatible-compositions?
            (with-key composed-function-components
              equal?)))
   (call (.. compose-powers
             (% ->power-function)))]
  [(and (all composed-function?)
        ~compatible-compositions?) ; compose at same level
   (struct-copy composed-function g
                [components (append (composed-function-components g)
                                    (composed-function-components h))])]
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

(define (compose #:compose-with [composer usual-composition]
                 . gs)
  (switch (gs)
          [empty? (function-null #:compose-with composer)]
          [(.. empty? rest) (call first)]
          [else
           (let ([gs (reverse gs)])
             (foldl function-compose
                    (first gs)
                    (rest gs)))]))

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

(define (~curry chirality func invocation-args)
  (if (and (composed-function? func) ;
           (curried-arguments? (function-applier func)))
      ;; application scheme is compatible so just apply the
      ;; new args to the existing scheme
      (composed-function (pass (function-applier func)
                               invocation-args
                               chirality)
                         chirality
                         (composed-function-components func)
                         (composed-function-composer func))
      ;; (pass-args func
      ;;            invocation-args
      ;;            chirality)
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

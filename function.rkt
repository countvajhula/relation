#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/match
         racket/generic
         racket/stream
         racket/hash
         racket/set
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
                  apply/kw-hash))

(require "private/util.rkt")

(provide lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         lambda.
         λ.
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
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct function ((components list?)
                            (composer monoid?)
                            (applier application-scheme?)
                            (chirality symbol?))]
          [struct partial-arguments
            ((left list?)
             (right list?)
             (kw hash?))]
          [struct template-arguments
            ((pos list?)
             (kw hash?))]
          [empty-partial-arguments partial-arguments?]
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
          [uncurry (functional/c)]
          [conjoin (variadic-function/c procedure? function?)]
          [&& (variadic-function/c procedure? function?)]
          [disjoin (variadic-function/c procedure? function?)]
          [|| (variadic-function/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]))

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

(define (~min-arity-value arity)
  (cond [(number? arity) arity]
        [(arity-at-least? arity) (arity-at-least-value arity)]
        [(list? arity) (apply min (map ~min-arity-value arity))]
        [else (raise-argument-error 'min-arity
                                    "normalized-arity?"
                                    arity)]))

(define (~min-arity f)
  (~min-arity-value (procedure-arity f)))

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

(define-generics application-scheme
  ;; apply-arguments accepts an arguments structure representing
  ;; args provided in a single invocation, and returns an updated
  ;; application-scheme instance
  (apply-arguments application-scheme args chirality)
  ;; flat-arguments compiles all previously supplied arguments
  ;; into a "flat" arguments structure that represents the
  ;; arguments for the invocation of the underlying function
  (flat-arguments application-scheme)
  ;; handle-failure is expected to either
  ;; 1. return a modified application scheme instance, OR
  ;; 2. raise an exception
  (handle-failure application-scheme exception)
  #:defaults
  ([arguments? (define (apply-arguments this args chirality)
                 (if (eq? chirality 'left)
                     (arguments-merge this args)
                     (arguments-merge args this)))
               (define (flat-arguments this)
                 this)
               (define (handle-failure this exception)
                 (raise exception))]))

(struct partial-arguments (left right kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (apply-arguments this args chirality)
     ;; incorporate fresh arguments into the partial application
     ;; retaining existing arg positions and appending the fresh ones
     ;; at the positions implied by the chirality
     (let ([left-args (if (eq? chirality 'left)
                          (append (partial-arguments-left this)
                                  (arguments-positional args))
                          (partial-arguments-left this))]
           [right-args (if (eq? chirality 'right)
                           ;; note order reversed for right args
                           (append (arguments-positional args)
                                   (partial-arguments-right this))
                           (partial-arguments-right this))])
       (partial-arguments left-args
                          right-args
                          (hash-union (partial-arguments-kw this)
                                      (arguments-keyword args)))))
   (define (flat-arguments this)
     (make-arguments (partial-arguments-positional this)
                     (partial-arguments-kw this)))
   (define (handle-failure this exception)
     this)]

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

(define empty-partial-arguments
  (partial-arguments null null (hash)))

(define (partial-arguments-positional args)
  (append (partial-arguments-left args)
          (partial-arguments-right args)))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(struct template-arguments (pos kw)
  #:transparent

  #:methods gen:application-scheme
  [(define (apply-arguments this args chirality)
     ;; need tests for these
     ;; clean up comments
     ;; merge into master
     (define arg-stack (apply make-stack (arguments-positional args)))
     (define filled-in-pos-template
       (for/list ([arg (template-arguments-pos this)])
         (if (just? arg)
             arg
             (if (stack-empty? arg-stack)
                 (raise-arguments-error
                  'apply-arguments
                  "Not enough positional arguments provided to template!")
                 (just (pop! arg-stack))))))
     (unless (stack-empty? arg-stack)
       (raise-arguments-error 'apply-arguments
                              "Too many arguments provided to template!"
                              "extra args" (stack->list arg-stack)))
     (define filled-in-kw-template
       (for/hash ([k (hash-keys (template-arguments-kw this))])
         (values k (hash-ref (arguments-keyword args) k nothing))))
     (for-each
      (λ (k)
        (unless (hash-has-key? (template-arguments-kw this) k)
          (raise-arguments-error
           'apply-arguments
           "Unexpected keyword argument provided to template!"
           "keyword" k)))
      (hash-keys (arguments-keyword args)))
     (hash-for-each
      filled-in-kw-template
      (λ (k v)
        (when (nothing? v)
          (raise-arguments-error 'apply-arguments
                                 "Missing keyword argument in template!"
                                 "keyword" k))))
     (template-arguments filled-in-pos-template
                         filled-in-kw-template))
   (define (flat-arguments this)
     (make-arguments (filter-just (template-arguments-pos this))
                     (template-arguments-kw this)))
   (define (handle-failure this exception)
     (raise exception))])

;; then get template arguments to work
;; profile before and after
;; tests needed:
;;  - partial/template
;; rename partial-arguments -> curried-arguments to differentiate from uncurried partial application
;; formalize composition of application schemes
(define (eval-function f args)
  ;; the happy path
  (let ([components (function-components f)]
        [composer (function-composer f)])
    (apply/arguments (apply composer components)
                     args)))

(define (eval-if-saturated f applier)
  (let* ([components (function-components f)]
         [leading-function (if (null? components)
                               (monoid-id (function-composer f))
                               (last components))]
         [args (flat-arguments applier)]
         [pos-args (arguments-positional args)]
         [kw-args (arguments-keyword args)])
    (with-handlers ([exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity leading-function))
                           (raise exn)
                           (struct-copy function f
                                        [applier (handle-failure applier exn)])))]
                    [exn:fail:contract?
                     ;; presence of a keyword argument results in a premature
                     ;; contract failure that's not the arity error, even though
                     ;; that's probably what it should be since providing additional
                     ;; positional arguments results in expected behavior
                     ;; additionally, also handle invalid keyword arg here
                     (λ (exn)
                       (let-values ([(req-kw opt-kw)
                                     (procedure-keywords leading-function)])
                         (if (or (empty? kw-args)
                                 ;; the arity error is masked in the presence of keyword
                                 ;; args so we check for it again here
                                 (> (length pos-args)
                                    (~min-arity leading-function))
                                 ;; any unexpected keywords?
                                 (any?
                                  (map (!! (in? (append req-kw opt-kw)))
                                       (hash-keys kw-args)))
                                 ;; all required arguments received?
                                 (and (subset? req-kw (hash-keys kw-args))
                                      (>= (length pos-args)
                                          (~min-arity leading-function))))
                             (raise exn)
                             (struct-copy function f
                                          [applier (handle-failure applier exn)]))))])
      (eval-function f args))))

(define (apply-function f args)
  (let* ([applier (function-applier f)]
         [updated-applier (apply-arguments applier
                                           args
                                           (function-chirality f))])
    (eval-if-saturated f updated-applier)))

(struct function (components
                  composer
                  applier
                  chirality)
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [args (make-arguments (rest (arguments-positional packed-args))
                                (arguments-keyword packed-args))])
     (apply-function self args)))

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
     (function (-rest (function-components self))
               (function-composer self)
               (function-applier self)
               (function-chirality self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-composer self)
               (function-applier self)
               (function-chirality self)))]

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
     (let* ([applier (function-applier self)]
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
                       #:apply-with [applier empty-partial-arguments]
                       #:curry-on [chirality 'left]
                       . fs)
  (function fs
            composer
            applier
            chirality))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:apply-with [applier empty-partial-arguments]
                                 #:curry-on [chirality 'left]
                                 . fs)
  (apply f
         #:compose-with composer
         #:apply-with applier
         #:curry-on chirality
         (reverse fs)))

(define f> make-threading-function)

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
                       #:apply-with [applier empty-partial-arguments])
  (make-function #:compose-with composer
                 #:apply-with applier))

(define (function-cons proc f)
  (function (cons proc (function-components f))
            (function-composer f)
            (function-applier f)
            (function-chirality f)))

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

(define compose f)


(define (~curry chirality f invocation-args)
  (if (and (function? f)
           (partial-arguments? (function-applier f)))
      ;; application scheme is compatible so just apply the
      ;; new args to the existing scheme
      (function (function-components f)
                (function-composer f)
                (apply-arguments (function-applier f)
                                 invocation-args
                                 chirality)
                chirality)
      ;; wrap the existing function with one that will be curried
      (function (list f)
                usual-composition
                (apply-arguments empty-partial-arguments
                                 invocation-args
                                 chirality)
                chirality)))

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

(define/arguments (partial/template args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)])
    (f #:apply-with (template-arguments pos kw)
       func)))

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

(define/arguments (partial args)
  (let* ([func (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (make-arguments pos kw)])
    (f #:apply-with invocation-args
       func)))

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

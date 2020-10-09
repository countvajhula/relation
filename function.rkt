#lang racket/base

(require (prefix-in f: racket/function)
         (except-in racket/contract/base
                    predicate/c)
         racket/match
         racket/generic
         racket/stream
         racket/hash
         racket/set
         arguments
         syntax/parse/define
         (prefix-in b: racket/base)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         (only-in data/functor
                  (map f:map))
         mischief/shorthand
         contract/social
         relation/logic
         relation/equivalence)

(provide lambda/function
         lambda/f
         λ/f
         define/function
         define/f
         lambda.
         λ.
         (contract-out
          [unthunk (-> procedure? any/c ... procedure?)]
          [if-f (-> (unconstrained-domain-> boolean?)
                    procedure?
                    procedure?
                    procedure?)]
          [true. (unconstrained-domain-> boolean?)]
          [false. (unconstrained-domain-> boolean?)]
          [arg (-> exact-nonnegative-integer? procedure?)]
          [flip functional/c]
          [flip$ functional/c]
          [flip* functional/c]
          [lift functional/c]
          [pack (-> procedure? any/c ... sequence?)]
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct function ((components list?)
                            (composer monoid?)
                            (side symbol?)
                            (left-args list?)
                            (right-args list?)
                            (kw-args hash?))]
          [make-function (->* ()
                              (#:compose-with monoid?
                               #:curry-on symbol?)
                              #:rest (listof procedure?)
                              function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?
                                         #:curry-on symbol?)
                                        #:rest (listof procedure?)
                                        function?)]
          [f (->* ()
                  (#:compose-with monoid?
                   #:curry-on symbol?)
                  #:rest (listof procedure?)
                  function?)]
          [f> (->* ()
                   (#:compose-with monoid?
                    #:curry-on symbol?)
                   #:rest (listof procedure?)
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?
                               #:curry-on symbol?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-arguments (-> function? arguments?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (variadic-constructor/c procedure? function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [conjoin (variadic-constructor/c procedure? function?)]
          [&& (variadic-constructor/c procedure? function?)]
          [disjoin (variadic-constructor/c procedure? function?)]
          [|| (variadic-constructor/c procedure? function?)]
          [negate (-> procedure? function?)]
          [!! (-> procedure? function?)]))

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

(define (eval-function f)
  (let ([components (function-components f)]
        [composer (function-composer f)])
    (apply/arguments (apply composer components)
                     (function-arguments f))))

(define (eval-if-saturated f)
  (let* ([components (function-components f)]
         [left-args (function-left-args f)]
         [right-args (function-right-args f)]
         [pos-args (append left-args right-args)]
         [kw-args (function-kw-args f)])
    (with-handlers ([exn:fail:contract:arity?
                     (λ (exn)
                       (if (> (length pos-args)
                              (~min-arity (last components)))
                           (raise exn)
                           f))]
                    [exn:fail:contract?
                     ;; presence of a keyword argument results in a premature
                     ;; contract failure that's not the arity error, even though
                     ;; that's probably what it should be since providing additional
                     ;; positional arguments results in expected behavior
                     ;; additionally, also handle invalid keyword arg here
                     (λ (exn)
                       (let-values ([(req-kw opt-kw)
                                     (procedure-keywords (last components))])
                         (if (or (empty? kw-args)
                                 ;; the arity error is masked in the presence of keyword
                                 ;; args so we check for it again here
                                 (> (length pos-args)
                                    (~min-arity (last components)))
                                 ;; any unexpected keywords?
                                 (any?
                                  (map (!! (in? (append req-kw opt-kw)))
                                       (hash-keys kw-args)))
                                 ;; all required arguments received?
                                 (and (subset? req-kw (hash-keys kw-args))
                                      (>= (length pos-args)
                                          (~min-arity (last components)))))
                             (raise exn)
                             f)))])
      (eval-function f))))

(struct function (components
                  composer
                  side
                  left-args
                  right-args
                  kw-args)
  ; maybe incorporate a power into the function type
  #:transparent

  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [side (function-side self)]
          [curry-proc (if (= side 'left)
                          curry
                          curryr)]
          [curried-f
           (curry-proc
            (apply/arguments curry-proc
                             packed-args))])
     (eval-if-saturated curried-f)))

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
               (function-side self)
               (function-left-args self)
               (function-right-args self)
               (function-kw-args self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-composer self)
               (function-side self)
               (function-left-args self)
               (function-right-args self)
               (function-kw-args self)))]

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
     (let* ([args (function-arguments self)]
            [components (function-components self)]
            [representation
             (list 'λ
                   (if (eq? (function-side self) 'left)
                       (list args '_)
                       (list '_ args))
                   (list* (match (function-composer self)
                            [(== usual-composition) '..]
                            [(== conjoin-composition) '&&]
                            [(== disjoin-composition) '||]
                            [_ '??])
                          components))])
       (recur representation port)))])

(define (make-function #:compose-with [composer usual-composition]
                       #:curry-on [side 'left]
                       . fs)
  (function fs
            composer
            side
            null
            null
            (hash)))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 #:curry-on [side 'left]
                                 . fs)
  (apply f
         #:compose-with composer
         #:curry-on side
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
                       #:curry-on [side 'left])
  (make-function #:compose-with composer
                 #:curry-on side))

(define (function-cons proc f)
  (function (cons proc (function-components f))
            (function-composer f)
            (function-side f)
            (function-left-args f)
            (function-right-args f)
            (function-kw-args f)))

(define (function-arguments f)
  (make-arguments (append (function-left-args f)
                          (function-right-args f))
                  (function-kw-args f)))

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
                             (stream)
                             (let ([v ((first remf) v)])
                               (stream-cons v
                                            (loop (rest remf)
                                                  v))))))))))

(define compose f)

(define/arguments (curry args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  'left
                  (append (function-left-args f)
                          pos)
                  (function-right-args f)
                  (hash-union (function-kw-args f)
                              kw))
        (function (list f)
                  usual-composition
                  'left
                  pos
                  null
                  kw))))

(define/arguments (curryr args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  'right
                  (function-left-args f)
                  (append pos
                          (function-right-args f))
                  (hash-union (function-kw-args f)
                              kw))
        (function (list f)
                  usual-composition
                  'right
                  null
                  pos
                  kw))))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

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

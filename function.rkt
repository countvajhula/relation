#lang racket/base

(require (prefix-in f: racket/function)
         racket/contract/base
         racket/match
         racket/generic
         racket/stream
         racket/hash
         arguments
         (prefix-in b: racket/base)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         (only-in data/functor
                  (map f:map))
         relation/equivalence)

(provide (contract-out
          [unthunk (-> procedure? any/c ... procedure?)]
          [if-f (-> (unconstrained-domain-> boolean?)
                    procedure?
                    procedure?
                    procedure?)]
          [true. (unconstrained-domain-> boolean?)]
          [false. (unconstrained-domain-> boolean?)]
          [arg (-> exact-nonnegative-integer? procedure?)]
          [flip (-> procedure? procedure?)]
          [flip$ (-> procedure? procedure?)]
          [flip* (-> procedure? procedure?)]
          [lift (-> procedure? function?)]
          [pack (-> procedure? any/c ... sequence?)]
          [struct monoid ((f procedure?)
                          (id procedure?))]
          [struct function ((components list?)
                            (composer monoid?)
                            (side symbol?)
                            (args arguments?))]
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
          [function-cons (-> procedure? function? function?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (-> procedure? ... function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [conjoin (-> procedure? ... function?)]
          [&& (-> procedure? ... function?)]
          [disjoin (-> procedure? ... function?)]
          [|| (-> procedure? ... function?)]
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

(define (eval-function f)
  (let ([components (function-components f)]
        [composer (function-composer f)]
        [args (function-args f)])
    (apply/arguments (apply composer components)
                     args)))

(define (eval-if-saturated f)
  (with-handlers ([exn:fail:contract:arity?
                   (λ (exn)
                     (let ([components (function-components f)]
                           [args (function-args f)])
                       (if (> (length (arguments-positional args))
                              (~min-arity (last components)))
                           (raise exn)
                           f)))])
    (eval-function f)))

(struct function (components
                  composer
                  side
                  args)
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
               (function-args self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-composer self)
               (function-side self)
               (function-args self)))]
  #:methods gen:countable
  [(define/generic -length length)
   (define (length self)
     (-length (function-components self)))])

(define (make-function #:compose-with [composer (monoid b:compose values)]
                       #:curry-on [side 'left]
                       . fs)
  (function fs
            composer
            side
            empty-arguments))

(define f make-function)

(define (make-threading-function #:compose-with [composer (monoid b:compose values)]
                                 #:curry-on [side 'left]
                                 . fs)
  (apply f
         #:compose-with composer
         #:curry-on side
         (reverse fs)))

(define f> make-threading-function)

(define (function-null #:compose-with [composer (monoid b:compose values)]
                       #:curry-on [side 'left])
  (make-function #:compose-with composer
                 #:curry-on side))

(define (function-cons proc f)
  (function (cons proc (function-components f))
            (function-composer f)
            (function-side f)
            (function-args f)))

(define (~fold-into-tail lst)
  (cond [(empty? lst) (raise-arguments-error 'fold-into-tail
                                             "Tail must be a list")]
        [(and (list? (first lst))
              (empty? (rest lst)))
         (first lst)]
        [else (cons (first lst)
                    (~fold-into-tail (rest lst)))]))

(define/arguments (apply/steps args)
  (let ([f (first (arguments-positional args))]
        [args (make-arguments (~fold-into-tail
                               (rest (arguments-positional args)))
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

(define (~curry f side args)
  (if (function? f)
      (function (function-components f)
                (function-composer f)
                side
                (arguments-merge (function-args f)
                                 args))
      (function (list f)
                (monoid b:compose
                        values)
                side
                args)))

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [args-invocation (make-arguments pos null kw)])
    (~curry f 'left args-invocation)))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [args-invocation (make-arguments null pos kw)])
    (~curry f 'right args-invocation)))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(define (conjoin . fs)
  (apply f
         #:compose-with (monoid f:conjoin
                                true.)
         fs))

(define (disjoin . fs)
  (apply f
         #:compose-with (monoid f:disjoin
                                false.)
         fs))

(define (negate g)
  (f not g))

(define && conjoin)
(define || disjoin)
(define !! negate)

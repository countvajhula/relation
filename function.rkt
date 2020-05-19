#lang racket/base

(require (prefix-in f: racket/function)
         racket/contract/base
         racket/match
         racket/generic
         racket/stream
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
          [flip (-> procedure? procedure?)]
          [flip$ (-> procedure? procedure?)]
          [flip* (-> procedure? procedure?)]
          [lift (-> procedure? function?)]
          [monoid? (-> any/c boolean?)]
          [monoid (-> procedure? any/c monoid?)]
          [monoid-f (-> monoid? procedure?)]
          [monoid-id (-> monoid? procedure?)]
          [function? (-> any/c boolean?)]
          [function (-> list?
                        monoid?
                        symbol?
                        arguments?
                        function?)]
          [function-components (-> function? list?)]
          [function-composer (-> function? (-> any/c any/c any/c))]
          [function-side (-> function? symbol?)]
          [function-args (-> function? arguments?)]
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
          [function-null function?]
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

(struct function (components composer side args)
  ; maybe incorporate a power into the function type
  #:transparent
  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [components (function-components self)]
          [composer (function-composer self)]
          [side (function-side self)]
          [pos (rest (arguments-positional packed-args))]
          [kw (arguments-keyword packed-args)]
          [args-invocation (make-arguments pos kw)]
          [args (if (= (function-side self) 'left)
                    (arguments-merge (function-args self)
                                     args-invocation)
                    (arguments-merge args-invocation
                                     (function-args self)))])
     (with-handlers ([exn:fail:contract:arity?
                      (λ (exn)
                        (if (> (length (arguments-positional args))
                               (~min-arity (last (function-components self))))
                            (raise exn)
                            (let ([curry-proc (if (= side 'left)
                                                  curry
                                                  curryr)])
                              (apply/arguments curry-proc
                                               (arguments-cons self args-invocation)))))])
       (apply/arguments (apply composer components)
                        args))))
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
                       #:curry-on [curry-on 'left]
                       . fs)
  (function fs
            composer
            curry-on
            empty-arguments))

(define (make-threading-function #:compose-with [composer (monoid b:compose values)]
                                 #:curry-on [curry-on 'left]
                                 . fs)
  (function (reverse fs)
            composer
            curry-on
            empty-arguments))

(define f make-function)

(define f> make-threading-function)

(define function-null (make-function))

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

(define/arguments (curry args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  (function-side f)
                  (arguments-merge (function-args f)
                                   (make-arguments pos kw)))
        (function (list f)
                  (monoid b:compose
                          values)
                  'left
                  (make-arguments pos kw)))))

(define/arguments (curryr args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  (function-side f)
                  (arguments-merge (make-arguments pos kw)
                                   (function-args f)))
        (function (list f)
                  (monoid b:compose
                          values)
                  'right
                  (make-arguments pos kw)))))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(define (conjoin . fs)
  (function fs
            (monoid f:conjoin
                    (f:const #t))
            'left
            empty-arguments))

(define (disjoin . fs)
  (function fs
            (monoid f:disjoin
                    (f:const #f))
            'left
            empty-arguments))

(define (negate g)
  (f not g))

(define && conjoin)
(define || disjoin)
(define !! negate)

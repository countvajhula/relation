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
          [andf (-> any/c ... any/c)]
          [orf (-> any/c ... any/c)]
          [iff (-> (-> any/c boolean?) procedure? procedure? procedure?)]
          [flip (-> procedure? procedure?)]
          [flip$ (-> procedure? procedure?)]
          [flip* (-> procedure? procedure?)]
          [lift (-> procedure? procedure?)]
          [function? (-> any/c boolean?)]
          [function (-> list?
                        (-> any/c any/c any/c)
                        procedure?
                        symbol?
                        arguments?
                        function?)]
          [function-components (-> function? list?)]
          [function-composer (-> function? (-> any/c any/c any/c))]
          [function-identity (-> function? procedure?)]
          [function-side (-> function? symbol?)]
          [function-args (-> function? arguments?)]
          [make-function (-> procedure? ... function?)]
          [make-right-function (-> procedure? ... function?)]
          [f (-> procedure? ... function?)]
          [f> (-> procedure? ... function?)]
          [function-null function?]
          [function-cons (-> procedure? function? function?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (-> (or/c function? procedure?) ... function?)]
          [power (-> integer? (or/c function? procedure?) function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [conjoin (-> procedure? ... function?)]
          [&& (-> procedure? ... function?)]
          [disjoin (-> procedure? ... function?)]
          [|| (-> procedure? ... function?)]
          [negated? (-> any/c boolean?)]
          [negated (-> procedure? negated?)]
          [negated-f (-> negated? procedure?)]
          [negate (-> procedure? negated?)]
          [!! (-> procedure? negated?)]))

(define (unthunk f . args)
  (λ ignored-args
    (apply f args)))

(define (andf . args)
  (match args
    ['() #t]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (and v (apply andf vs))])]))

(define (orf . args)
  (match args
    ['() #f]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (or v (apply orf vs))])]))

(define (iff pred f g)
  (λ (v)
    (if (pred v)
        (f v)
        (g v))))

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

(struct function (components composer identity side args)
  #:transparent
  #:property prop:procedure
  (lambda/arguments
   packed-args
   (let* ([self (first (arguments-positional packed-args))]
          [components (function-components self)]
          [composer (function-composer self)]
          [identity (function-identity self)]
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
       (let ([f (foldl (flip composer)
                       identity
                       components)])
         (apply/arguments f args)))))
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
               (function-identity self)
               (function-side self)
               (function-args self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-composer self)
               (function-identity self)
               (function-side self)
               (function-args self)))])

(define (make-function . fs)
  (function fs
            b:compose
            values
            'left
            empty-arguments))

(define (make-right-function . fs)
  (function fs
            b:compose
            values
            'right
            empty-arguments))

(define f make-function)

(define f> make-right-function)

(define function-null (make-function))

(define (function-cons proc f)
  (function (cons proc (function-components f))
            (function-composer f)
            (function-identity f)
            (function-side f)
            (function-args f)))

(define (fold-into-tail lst)
  (cond [(empty? lst) (raise-arguments-error 'fold-into-tail
                                             "Tail must be a list")]
        [(and (list? (first lst))
              (empty? (rest lst)))
         (first lst)]
        [else (cons (first lst)
                    (fold-into-tail (rest lst)))]))

(define/arguments (apply/steps args)
  (let ([f (first (arguments-positional args))]
        [args (make-arguments (fold-into-tail
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

(define (compose . fs)
  (apply f fs))

(define (~power n f)
  ; maybe incorporate a power into the function type
  (if (= n 0)
      function-null
      (function-cons f (~power (sub1 n) f))))

(define power (f ~power))

(define/arguments (curry args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  (function-identity f)
                  (function-side f)
                  (arguments-merge (function-args f)
                                   (make-arguments pos kw)))
        (function (list f)
                  b:compose
                  values
                  'left
                  (make-arguments pos kw)))))

(define/arguments (curryr args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (if (function? f)
        (function (function-components f)
                  (function-composer f)
                  (function-identity f)
                  (function-side f)
                  (arguments-merge (make-arguments pos kw)
                                   (function-args f)))
        (function (list f)
                  b:compose
                  values
                  'right
                  (make-arguments pos kw)))))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(define (conjoin . fs)
  (function fs
            f:conjoin
            (f:const #t)
            'left
            empty-arguments))

(define (disjoin . fs)
  (function fs
            f:disjoin
            (f:const #f)
            'left
            empty-arguments))

(struct negated (f)
  #:transparent
  #:property prop:procedure
  (λ (self . args)
    (not (apply (negated-f self) args))))

(define (negate f)
  (negated f))

(define && conjoin)
(define || disjoin)
(define !! negate)

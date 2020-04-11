#lang racket/base

(require (prefix-in f: racket/function)
         racket/contract/base
         racket/match
         racket/generic
         arguments
         (prefix-in b: racket/base)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         relation/equivalence)

(provide &&
         ||
         !!
         curry
         curryr
         (contract-out
          [unthunk (-> procedure? any/c ... procedure?)]
          [andf (-> any/c ... any/c)]
          [orf (-> any/c ... any/c)]
          [iff (-> (-> any/c boolean?) procedure? procedure? procedure?)]
          [flip (-> procedure? procedure?)]
          [flip$ (-> procedure? procedure?)]
          [flip* (-> procedure? procedure?)]
          [function? (-> any/c boolean?)]
          [function (-> list? function?)]
          [function-components (-> function? list?)]
          [make-function (-> procedure? ... function?)]
          [function-null function?]
          [function-cons (-> procedure? function? function?)]
          [function-compose (-> (or/c function? procedure?) ... function?)]
          [conjoined? (-> any/c boolean?)]
          [conjoined (-> list? conjoined?)]
          [conjoined-components (-> conjoined? list?)]
          [conjoin (-> procedure? ... conjoined?)]
          [disjoined? (-> any/c boolean?)]
          [disjoined (-> list? disjoined?)]
          [disjoined-components (-> disjoined? list?)]
          [disjoin (-> procedure? ... disjoined?)]
          [negated? (-> any/c boolean?)]
          [negated (-> procedure? disjoined?)]
          [negated-f (-> disjoined? procedure?)]
          [negate (-> procedure? disjoined?)]
          [curried? (-> any/c boolean?)]))

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

(struct function (components)
  #:transparent
  #:property prop:procedure
  (λ (self . args)
    (apply (apply compose
                  (function-components self))
           args))
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
     (function (-rest (function-components self))))
   (define (reverse self)
     (function (-reverse (function-components self))))])

(define (make-function . fs)
  (function fs))

(define function-null (make-function))

(define (function-cons proc fs)
  (function (cons proc (function-components fs))))

(define (function-compose . fs)
  (function (apply b:append
                   (b:map (iff function?
                               function-components
                               list)
                          fs))))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

(struct conjoined (components)
  #:transparent
  #:property prop:procedure
  (λ (self . args)
    (apply (apply f:conjoin
                  (conjoined-components self))
           args))
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (conjoined-components self)))
   (define (first self)
     (-first (conjoined-components self)))
   (define (rest self)
     (conjoined (-rest (conjoined-components self))))
   (define (reverse self)
     (conjoined (-reverse (conjoined-components self))))])

(define (conjoin . fs)
  (conjoined fs))

(struct disjoined (components)
  #:transparent
  #:property prop:procedure
  (λ (self . args)
    (apply (apply f:disjoin
                  (disjoined-components self))
           args))
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (empty? self)
     (-empty? (disjoined-components self)))
   (define (first self)
     (-first (disjoined-components self)))
   (define (rest self)
     (disjoined (-rest (disjoined-components self))))
   (define (reverse self)
     (disjoined (-reverse (disjoined-components self))))])

(define (disjoin . fs)
  (disjoined fs))

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

(struct curried (f side args)
  #:transparent
  #:property prop:procedure
  (lambda/arguments
   args
   (let* ([self (first (arguments-positional args))]
          [pos (rest (arguments-positional args))]
          [kw (arguments-keyword args)]
          [args-invocation (make-arguments pos kw)])
     (let ([f (curried-f self)]
           [side (curried-side self)]
           [args (if (= (curried-side self) 'left)
                     (arguments-merge (curried-args self)
                                      args-invocation)
                     (arguments-merge args-invocation
                                      (curried-args self)))])
       (with-handlers ([exn:fail:contract:arity?
                        (λ (exn)
                          (let ([curry-proc (if (= side 'left)
                                                curry
                                                curryr)])
                            (apply/arguments curry-proc
                                             (arguments-cons f args))))])
         (apply/arguments f args))))))

(define/arguments (curry args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (curried f
             'left
             (make-arguments pos kw))))

(define/arguments (curryr args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (curried f
             'right
             (make-arguments pos kw))))

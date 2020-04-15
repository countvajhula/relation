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
         relation/equivalence)

(provide (contract-out
          [unthunk (-> procedure? any/c ... procedure?)]
          [andf (-> any/c ... any/c)]
          [orf (-> any/c ... any/c)]
          [iff (-> (-> any/c boolean?) procedure? procedure? procedure?)]
          [flip (-> procedure? procedure?)]
          [flip$ (-> procedure? procedure?)]
          [flip* (-> procedure? procedure?)]
          [function? (-> any/c boolean?)]
          [function (-> list? symbol? arguments? function?)]
          [function-components (-> function? list?)]
          [function-side (-> function? symbol?)]
          [function-args (-> function? arguments?)]
          [make-function (-> procedure? ... function?)]
          [make-right-function (-> procedure? ... function?)]
          [f (-> procedure? ... function?)]
          [f> (-> procedure? ... function?)]
          [function-null function?]
          [function-cons (-> procedure? function? function?)]
          [compose (-> (or/c function? procedure?) ... function?)]
          [power (-> integer? (or/c function? procedure?) function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [conjoined? (-> any/c boolean?)]
          [conjoined (-> list? conjoined?)]
          [conjoined-components (-> conjoined? list?)]
          [conjoin (-> procedure? ... conjoined?)]
          [&& (-> procedure? ... conjoined?)]
          [disjoined? (-> any/c boolean?)]
          [disjoined (-> list? disjoined?)]
          [disjoined-components (-> disjoined? list?)]
          [disjoin (-> procedure? ... disjoined?)]
          [|| (-> procedure? ... disjoined?)]
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

(struct function (components side args)
  #:transparent
  #:property prop:procedure
  (lambda/arguments
   args
   (let* ([self (first (arguments-positional args))]
          [pos (rest (arguments-positional args))]
          [kw (arguments-keyword args)]
          [args-invocation (make-arguments pos kw)])
     (let ([f (apply b:compose (function-components self))]
           [side (function-side self)]
           [args (if (= (function-side self) 'left)
                     (arguments-merge (function-args self)
                                      args-invocation)
                     (arguments-merge args-invocation
                                      (function-args self)))])
       (with-handlers ([exn:fail:contract:arity?
                        (λ (exn)
                          (let ([curry-proc (if (= side 'left)
                                                curry
                                                curryr)])
                            (apply/arguments curry-proc
                                             (arguments-cons f args))))])
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
               (function-side self)
               (function-args self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-side self)
               (function-args self)))])

(define (make-function . fs)
  (function fs 'left empty-arguments))

(define (make-right-function . fs)
  (function fs 'right empty-arguments))

(define f make-function)

(define f> make-right-function)

(define function-null (make-function))

(define (function-cons proc f)
  (function (cons proc (function-components f))
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
    (function (list f)
              'left
              (make-arguments pos kw))))

(define/arguments (curryr args)
  (let ([f (first (arguments-positional args))]
        [pos (rest (arguments-positional args))]
        [kw (arguments-keyword args)])
    (function (list f)
              'right
              (make-arguments pos kw))))

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

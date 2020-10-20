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
                  reverse)
         (only-in data/functor
                  (map f:map))
         mischief/shorthand
         contract/social
         relation/logic
         (only-in relation/equivalence
                  in?))

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
                            (args partial-arguments?))]
          [struct partial-arguments
            ((side symbol?)
             (left list?)
             (right list?)
             (kw hash?))]
          [make-function (->* ()
                              (#:compose-with monoid?)
                              #:rest (listof procedure?)
                              function?)]
          [make-threading-function (->* ()
                                        (#:compose-with monoid?)
                                        #:rest (listof procedure?)
                                        function?)]
          [f (->* ()
                  (#:compose-with monoid?)
                  #:rest (listof procedure?)
                  function?)]
          [f> (->* ()
                   (#:compose-with monoid?)
                   #:rest (listof procedure?)
                   function?)]
          [function-null (->* ()
                              (#:compose-with monoid?)
                              function?)]
          [function-cons (binary-constructor/c procedure? function?)]
          [function-combined-arguments (function/c function? arguments?)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (variadic-constructor/c procedure? function?)]
          [curry (unconstrained-domain-> function?)]
          [curryr (unconstrained-domain-> function?)]
          [conjoin (variadic-constructor/c procedure? function?)]
          [&& (variadic-constructor/c procedure? function?)]
          [disjoin (variadic-constructor/c procedure? function?)]
          [|| (variadic-constructor/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]))

(module+ test
  (require rackunit))

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
        [composer (function-composer f)]
        [args (function-combined-arguments f)])
    (apply/arguments (apply composer components)
                     args)))

(define (eval-if-saturated f)
  (let* ([components (function-components f)]
         [args (function-args f)]
         [pos-args (partial-arguments-positional args)]
         [kw-args (partial-arguments-kw args)])
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

(define (kwhash->altlist v)
  (foldr (λ (a b)
           (list* (car a) (cdr a) b))
         null
         (sort (hash->list v)
               (λ (a b)
                 (keyword<? (car a) (car b))))))

(module+ test
  (test-case
      "kwhash->altlist"
    (check-equal? (kwhash->altlist (hash '#:c 2 '#:a 1 '#:b 3)) '(#:a 1 #:b 3 #:c 2))
    (check-equal? (kwhash->altlist (hash '#:a 1)) '(#:a 1))
    (check-equal? (kwhash->altlist (hash)) '())))

(struct partial-arguments (side left right kw)
  #:transparent

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (let ([side (partial-arguments-side self)]
           [left (partial-arguments-left self)]
           [right (partial-arguments-right self)]
           [kw (partial-arguments-kw self)])
       (if (or (null? left)
               (null? right))
           (recur (partial-arguments->arguments self) port)
           (recur (append left
                          (list '_)
                          right
                          (kwhash->altlist kw))
                  port))))])

(define empty-partial-arguments
  (partial-arguments 'left null null (hash)))

(define (apply-function f args)
  (let* ([side (partial-arguments-side (function-args f))]
         [curry-proc (if (eq? side 'left)
                         curry
                         curryr)]
         [curried-f
          (curry-proc
           (apply/arguments curry-proc
                            (arguments-cons f args)))])
    (eval-if-saturated curried-f)))

(define (partial-arguments-positional args)
  (append (partial-arguments-left args)
          (partial-arguments-right args)))

(define (partial-arguments->arguments args)
  (make-arguments (partial-arguments-positional args)
                  (partial-arguments-kw args)))

(struct function (components
                  composer
                  args)
  ; maybe incorporate a power into the function type
  ; probably better to define a higher-level `function-power` type
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
               (function-args self)))
   (define (reverse self)
     (function (-reverse (function-components self))
               (function-composer self)
               (function-args self)))]

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
     (let* ([args (function-args self)]
            [components (function-components self)]
            [composer (function-composer self)]
            [representation
             (list 'λ
                   args
                   (list* (match composer
                            [(== usual-composition) '..]
                            [(== conjoin-composition) '&&]
                            [(== disjoin-composition) '||]
                            [_ '??])
                          components))])
       (recur representation port)))])

(define (make-function #:compose-with [composer usual-composition]
                       . fs)
  (function fs
            composer
            empty-partial-arguments))

(define f make-function)

(define (make-threading-function #:compose-with [composer usual-composition]
                                 . fs)
  (apply f
         #:compose-with composer
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

(define (function-null #:compose-with [composer usual-composition])
  (make-function #:compose-with composer))

(define (function-cons proc f)
  (function (cons proc (function-components f))
            (function-composer f)
            (function-args f)))

(define (function-combined-arguments f)
  (partial-arguments->arguments (function-args f)))

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

(define (merge-partial-arguments a b)
  ;; merge arg sets, with arg set a prioritized over b
  ;; and using b's currying direction
  (partial-arguments (partial-arguments-side b)
                     (append (partial-arguments-left a)
                             (partial-arguments-left b))
                     ;; note order reversed for right args
                     (append (partial-arguments-right b)
                             (partial-arguments-right a))
                     (hash-union (partial-arguments-kw a)
                                 (partial-arguments-kw b))))

(define (~curry f invocation-args)
  (if (function? f)
      (function (function-components f)
                (function-composer f)
                (merge-partial-arguments (function-args f)
                                         invocation-args))
      (function (list f)
                usual-composition
                (merge-partial-arguments empty-partial-arguments
                                         invocation-args))))

(define/arguments (curry args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (partial-arguments 'left pos null kw)])
    (~curry f invocation-args)))

(define/arguments (curryr args)
  (let* ([f (first (arguments-positional args))]
         [pos (rest (arguments-positional args))]
         [kw (arguments-keyword args)]
         [invocation-args (partial-arguments 'right null pos kw)])
    (~curry f invocation-args)))

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

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
         relation/function/types
         "private/util.rkt")


(provide (all-from-out
          relation/function/application-scheme
          relation/function/types)
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
          [uncurry (functional/c)]
          [apply/steps (unconstrained-domain-> sequence?)]
          [compose (variadic-function/c procedure? function?)]
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

(define-simple-macro (lambda. v ...
                              (~or* (~datum ->) (~datum →))
                              body:expr ...)
  (lambda/f (v ...)
    body ...))

(define-alias λ. lambda.)

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

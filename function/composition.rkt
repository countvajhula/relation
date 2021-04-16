#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/list
         contract/social
         syntax/on
         "types.rkt")

(provide (contract-out
          [compose (variadic-function/c procedure? function?)]
          [conjoin (variadic-function/c procedure? function?)]
          [&& (variadic-function/c procedure? function?)]
          [disjoin (variadic-function/c procedure? function?)]
          [|| (variadic-function/c procedure? function?)]
          [negate (function/c procedure? function?)]
          [!! (function/c procedure? function?)]))

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

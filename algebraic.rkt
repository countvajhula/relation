#lang racket

(require (prefix-in b: racket/base)
         racket/vector
         racket/set
         racket/dict
         racket/generic
         data/collection
         relation/transform)

(provide gen:group
         group/c
         gen:monoid
         monoid/c
         (contract-out
          [group? (-> any/c boolean?)]
          [monoid? (-> any/c boolean?)]
          [+ (-> group? group? ... group?)]
          [.. (-> monoid? monoid? ... monoid?)]
          [∘ (-> monoid? monoid? ... monoid?)]))

(define-generics group
  (+ group . others)
  #:defaults ([number?
               (define (+ group . others)
                 (apply b:+ (cons group others)))]
              [vector?
               (define/generic generic-+ +)
               (define (+ group . others)
                 (->vector
                  (apply map generic-+ (cons group others))))]))

(define-generics monoid
  (.. monoid . others)
  #:defaults ([number?
               (define (.. monoid . others)
                 (apply b:* (cons monoid others)))]
              [string?
               (define (.. monoid . others)
                 (apply string-append (cons monoid others)))]
              [bytes?
               (define (.. monoid . others)
                 (apply bytes-append (cons monoid others)))]
              [list?
               (define (.. monoid . others)
                 (apply b:append (cons monoid others)))]
              [vector?
               (define (.. monoid . others)
                 (apply vector-append (cons monoid others)))]
              [set?
               (define (.. monoid . others)
                 (apply set-union (cons monoid others)))]
              [dict?
               (define (.. monoid . others)
                 (make-hash (sequence->list (apply append (cons monoid others)))))]
              [sequence?
               (define (.. monoid . others)
                 (apply append (cons monoid others)))]
              [procedure?
               (define (.. monoid . others)
                 (apply compose (cons monoid others)))]))

(define ∘ ..)

#lang racket

(require (prefix-in b: racket/base)
         racket/vector
         racket/set
         racket/generic
         data/collection)

(provide gen:group
         group?
         group/c
         gen:monoid
         monoid?
         monoid/c
         +
         ..
         ∘)

(define-generics group
  (+ group . others)
  #:defaults ([number?
               (define (+ group . others)
                 (apply b:+ (cons group others)))]
              [string?
               (define (+ group . others)
                 (error "No canonical group operation defined for " string?))]
              [bytes?
               (define (+ group . others)
                 (error "No canonical group operation defined for " bytes?))]
              [list?
               (define (+ group . others)
                 (error "No canonical group operation defined for " list?))]
              [vector?
               (define/generic generic-+ +)
               (define (+ group . others)
                 (apply map generic-+ (cons group others)))]
              [sequence?
               (define (+ group . others)
                 (error "No canonical group operation defined for " sequence?))]))

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
              [sequence?
               (define (.. monoid . others)
                 (apply append (cons monoid others)))]
              [procedure?
               (define (.. monoid . others)
                 (apply compose (cons monoid others)))]))

(define ∘ ..)

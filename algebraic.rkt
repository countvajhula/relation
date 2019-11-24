#lang racket

(require (prefix-in b: racket/base)
         racket/vector
         racket/generic
         data/collection)

(provide +
         *
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
               (define (+ group . others)
                 (apply map b:+ (cons group others)))]
              [sequence?
               (define (+ group . others)
                 (error "No canonical group operation defined for " sequence?))]))

(define-generics monoid
  (* monoid . others)
  #:defaults ([number?
               (define (* monoid . others)
                 (apply b:* (cons monoid others)))]
              [string?
               (define (* monoid . others)
                 (apply string-append (cons monoid others)))]
              [bytes?
               (define (* monoid . others)
                 (apply bytes-append (cons monoid others)))]
              [list?
               (define (* monoid . others)
                 (apply b:append (cons monoid others)))]
              [vector?
               (define (* monoid . others)
                 (apply vector-append (cons monoid others)))]
              [sequence?
               (define (* monoid . others)
                 (apply append (cons monoid others)))]
              [procedure?
               (define (* monoid . others)
                 (apply compose (cons monoid others)))]))

(define ∘ *)

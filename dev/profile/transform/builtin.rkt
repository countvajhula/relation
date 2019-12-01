#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         data/collection)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-string-to-number how-many)
  (for ([i (take how-many (in (cycle '("1" "2" "3"))))])
    (string->number i)))

(check-string-to-number 10000)

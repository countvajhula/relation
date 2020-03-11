#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         (except-in data/collection
                    foldl
                    foldl/steps)
         relation)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-string-to-number how-many)
  (for ([i (take how-many (in (cycle '("1" "2" "3"))))])
    (->number i)))

(define (check-number-to-string how-many)
  (for ([i (take how-many (in (cycle '(1 2 3))))])
    (->string i)))

(define (check-list-to-vector how-many)
  (for ([i (take how-many (in (cycle '((1 2 3) (4 5 6) (7 8 9)))))])
    (->vector i)))

(define (check-vector-to-list how-many)
  (for ([i (take how-many (in (cycle '(#(1 2 3) #(4 5 6) #(7 8 9)))))])
    (->list i)))

(check-string-to-number 10000)
(check-number-to-string 10000)
(check-list-to-vector 10000)
(check-vector-to-list 10000)

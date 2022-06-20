#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         racket/vector
         data/collection)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-string-to-number how-many)
  (for ([i (take how-many (in (cycle '("1" "2" "3"))))])
    (string->number i)))

(define (check-number-to-string how-many)
  (for ([i (take how-many (in (cycle '(1 2 3))))])
    (number->string i)))

(define (check-list-to-vector how-many)
  (for ([i (take how-many (in (cycle '((1 2 3) (4 5 6) (7 8 9)))))])
    (list->vector i)))

(define (check-vector-to-list how-many)
  (for ([i (take how-many (in (cycle '(#(1 2 3) #(4 5 6) #(7 8 9)))))])
    (vector->list i)))

(define (check-make-pairs how-many)
  (for ([i (take how-many (in (cycle '((a . 1) (b . 2) (c . 3)))))])
    (cons (car i) (cdr i))
    (cons (cdr i) (car i))))

(define (check-make-lists how-many)
  (for ([j (take how-many (in (cycle '(1 2 3))))])
    (cons j (list 5 6 7))
    (cons j (list 6 7 8))
    (cons j (list 7 8 9))))

(define (check-make-vectors how-many)
  (for ([j (take how-many (in (cycle '(1 2 3))))])
    (vector->immutable-vector
     (vector-append #(5 6 7) (vector j)))
    (vector->immutable-vector
     (vector-append #(6 7 8) (vector j)))
    (vector->immutable-vector
     (vector-append #(7 8 9) (vector j)))))

(define (check-make-hashes how-many)
  (for ([j (take how-many (in (cycle '((a . 1) (b . 2) (c . 3)))))])
    (hash-set (hash 'd 4) (car j) (cdr j))))

(check-string-to-number 10000)
(check-number-to-string 10000)
(check-list-to-vector 10000)
(check-vector-to-list 10000)
(check-make-pairs 10000)
(check-make-lists 10000)
(check-make-vectors 10000)
(check-make-hashes 10000)

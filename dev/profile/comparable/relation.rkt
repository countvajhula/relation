#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         data/collection
         relation)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-numbers how-many)
  (for ([i (take how-many (in (cycle '(1 1 3))))]
        [j (take how-many (in (cycle '(1 3 1))))])
    (< i j)
    (<= i j)
    (= i j)
    (>= i j)
    (> i j)
    (/= i j)))

(define (check-strings how-many)
  (for ([i (take how-many (in (cycle '("apple" "apple" "banana"))))]
        [j (take how-many (in (cycle '("apple" "banana" "apple"))))])
    (< i j)
    (<= i j)
    (= i j)
    (>= i j)
    (> i j)
    (/= i j)))

(define (check-chars how-many)
  (for ([i (take how-many (in (cycle '(#\a #\a #\b))))]
        [j (take how-many (in (cycle '(#\a #\b #\a))))])
    (< i j)
    (<= i j)
    (= i j)
    (>= i j)
    (> i j)
    (/= i j)))

(define (sort-numbers how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (sort i <)))

(define (sort-strings how-many)
  (for ([i (take how-many (in (cycle '(("dragonfruit" "apple" "cherry" "banana" "guava" "apple" "cherry" "elderberry")))))])
    (sort i <)))

(check-numbers 10000)
(check-strings 10000)
(check-chars 10000)
(sort-numbers 10000)
(sort-strings 10000)
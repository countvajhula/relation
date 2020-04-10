#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         racket/function
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         relation)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-numbers how-many)
  (for ([i (take how-many (in (cycle '(1 1 3))))]
        [j (take how-many (in (cycle '(1 3 1))))])
    (< i j)
    (≤ i j)
    (= i j)
    (≥ i j)
    (> i j)
    (≠ i j)))

(define (check-strings how-many)
  (for ([i (take how-many (in (cycle '("apple" "apple" "banana"))))]
        [j (take how-many (in (cycle '("apple" "banana" "apple"))))])
    (< i j)
    (≤ i j)
    (= i j)
    (≥ i j)
    (> i j)
    (≠ i j)))

(define (check-chars how-many)
  (for ([i (take how-many (in (cycle '(#\a #\a #\b))))]
        [j (take how-many (in (cycle '(#\a #\b #\a))))])
    (< i j)
    (≤ i j)
    (= i j)
    (≥ i j)
    (> i j)
    (≠ i j)))

(define (sort-numbers how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (sort < i)))

(define (sort-strings how-many)
  (for ([i (take how-many (in (cycle '(("dragonfruit" "apple" "cherry" "banana" "guava" "apple" "cherry" "elderberry")))))])
    (sort < i)))

(define (check-group-by how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (group-by #:key identity i)))

(define (check-min how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (apply min i)))

(define (check-max how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (apply max i)))

(check-numbers 10000)
(check-strings 10000)
(check-chars 10000)
(sort-numbers 10000)
(sort-strings 10000)
(check-group-by 10000)
(check-min 10000)
(check-max 10000)

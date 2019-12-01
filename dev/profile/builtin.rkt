#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         data/collection)

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
    (not (= i j))))

(define (check-strings how-many)
  (for ([i (take how-many (in (cycle '("apple" "apple" "banana"))))]
        [j (take how-many (in (cycle '("apple" "banana" "apple"))))])
    (string<? i j)
    (string<=? i j)
    (string=? i j)
    (string>=? i j)
    (string>? i j)
    (not (string=? i j))))

(define (check-chars how-many)
  (for ([i (take how-many (in (cycle '(#\a #\a #\b))))]
        [j (take how-many (in (cycle '(#\a #\b #\a))))])
    (char<? i j)
    (char<=? i j)
    (char=? i j)
    (char>=? i j)
    (char>? i j)
    (not (char=? i j))))

(check-numbers 10000)
(check-strings 10000)
(check-chars 10000)

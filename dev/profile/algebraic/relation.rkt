#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         relation)

(define (randoms n)
  (let ([num (random n)])
    (stream-cons num (randoms n))))

(define (check-+-numbers how-many)
  (for ([i (take how-many (in (cycle '(1 2 3))))]
        [j (take how-many (in (cycle '(1 2 3))))])
    (+ i j)))

(define (check-..-strings how-many)
  (for ([i (take how-many (in (cycle '("aaa" "bbb" "ccc"))))]
        [j (take how-many (in (cycle '("bbb" "aaa" "ccc"))))])
    (.. i j)))

(define (check-..-lists how-many)
  (for ([i (take how-many (in (cycle '((1 2 3) (4 5 6) (7 8 9)))))]
        [j (take how-many (in (cycle '((4 5 6) (1 2 3) (7 8 9)))))])
    (.. i j)))

(define (check-fold how-many)
  (for ([i (take how-many (in (cycle '((4 1 3 2 7 1 3 5)))))])
    (foldl + i #:into 0)))

(check-+-numbers 10000)
(check-..-strings 10000)
(check-..-lists 10000)
(check-fold 10000)

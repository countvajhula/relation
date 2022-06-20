#lang racket/base

(require (prefix-in b: racket/base)
         racket/stream
         racket/function
         data/collection)

(define (str-append-3 x y z)
  (string-append x y z))

(define (square x)
  (expt x 2))

(define (times2 x)
  (* x 2))

(define (check-curry-+ how-many)
  (for ([i (take how-many (in (cycle '(1 2 3))))]
        [j (take how-many (in (cycle '(1 2 3))))])
    ((curry + i j) 5)
    ((curryr + i j) 5)))

(define (check-curry-strings how-many)
  (for ([i (take how-many (in (cycle '("aaa" "bbb" "ccc"))))]
        [j (take how-many (in (cycle '("bbb" "aaa" "ccc"))))])
    (((curry str-append-3 i) j) "ddd")
    (((curryr str-append-3 i) j) "ddd")))

(define (check-compose how-many)
  (for ([i (take how-many (in (cycle '(1 2 3))))]
        [j (take how-many (in (cycle '(1 2 3))))])
    ((compose add1 square times2 +) 5 6)))

(check-curry-+ 10000)
(check-curry-strings 10000)
(check-compose 10000)

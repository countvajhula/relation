#lang racket/base

(module+ test
  (require rackunit
           racket/undefined
           relation))

(module+ test
  (check-true (undefined? undefined))
  (check-false (undefined? 5))
  (check-false (let ([v "hello"])
                 (undefined? v)))
  (check-equal? (andf 1 2 3) (and 1 2 3))
  (check-equal? (andf 1 #f 3) (and 1 #f 3))
  (check-equal? (andf #f #t) (and #f #t))
  (check-equal? (andf #t #t) (and #t #t))
  (check-equal? (orf 1 2 3) (or 1 2 3))
  (check-equal? (orf 1 #f 3) (or 1 #f 3))
  (check-equal? (orf #f #t) (or #f #t))
  (check-equal? (orf #t #t) (or #t #t))
  (check-equal? (all? (list 1 2 3)) (and 1 2 3))
  (check-equal? (all? (list 1 #f 3)) (and 1 #f 3))
  (check-equal? (all? (list #f #t)) (and #f #t))
  (check-equal? (all? (list #t #t)) (and #t #t))
  (check-equal? (any? (list 1 2 3)) (or 1 2 3))
  (check-equal? (any? (list 1 #f 3)) (or 1 #f 3))
  (check-equal? (any? (list #f #t)) (or #f #t))
  (check-equal? (any? (list #t #t)) (or #t #t))
  (check-equal? (none? (list 1 2 3)) (not (or 1 2 3)))
  (check-equal? (none? (list 1 #f 3)) (not (or 1 #f 3)))
  (check-equal? (none? (list #f #t)) (not (or #f #t)))
  (check-equal? (none? (list #t #t)) (not (or #t #t))))

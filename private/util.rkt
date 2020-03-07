#lang racket/base

(require racket/list)

(provide check-pairwise)

(define (check-pairwise check? vals)
  (let ([current (car vals)]
        [remaining (cdr vals)])
    (if (empty? remaining)
        #t
        (and (check? current (car remaining))
             (check-pairwise check? remaining)))))

#lang racket/base

(require (prefix-in b: racket/base)
         (except-in racket/list
                    take
                    group-by)
         (only-in data/collection
                  take
                  in
                  cycle)
         relation)

(define (check-booleans how-many)
  (for ([i (take how-many (in (cycle '(#f #t #f))))]
        [j (take how-many (in (cycle '(#t #f))))]
        [k (take how-many (in (cycle '(#t #f #t #f #t))))])
    (let ([vals (list i j k)])
      (all? vals)
      (any? vals))))

(check-booleans 10000)

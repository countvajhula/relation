#lang racket/base

(require (prefix-in b: racket/base)
         (only-in racket/list
                  group-by)
         (except-in racket/list
                    take)
         (only-in data/collection
                  take
                  in
                  cycle))

(define (check-booleans how-many)
  (for ([i (take how-many (in (cycle '(#f #t #f))))]
        [j (take how-many (in (cycle '(#t #f))))]
        [k (take how-many (in (cycle '(#t #f #t #f #t))))])
    (let ([vals (list i j k)])
      (let loop ([vs vals])
        (if (empty? vs)
            #t
            (and (first vs)
                 (loop (rest vs)))))
      (let loop ([vs vals])
        (if (empty? vs)
            #f
            (or (first vs)
                (loop (rest vs))))))))

(check-booleans 10000)

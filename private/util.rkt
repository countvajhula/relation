#lang racket/base

(require racket/match
         racket/function
         racket/stream
         data/collection)

(provide check-pairwise
         zip-with)

(define (check-pairwise check? vals)
  (if (empty? vals)
      #t
      (let ([v (first vals)]
            [vs (rest vals)])
        (if (empty? vs)
            #t
            (and (check? v (first vs))
                 (check-pairwise check? vs))))))

(define (orf . args)
  (match args
    ['() #f]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (or v (apply orf vs))])]))

(define any? (curry foldl
                    orf
                    #f))

(define (zip-with op . seqs)
  (if (any? (map empty? seqs))
      (stream)
      (let ([vs (map first seqs)])
        (stream-cons (apply op vs)
                     (apply zip-with op (map rest seqs))))))

#lang racket/base

(require racket/list
         racket/stream
         racket/match
         (prefix-in d: data/collection)
         relation/logic)

(provide check-pairwise
         exists
         for-all
         kwhash->altlist
         interleave)

(module+ test
  (require rackunit))

(define (check-pairwise check? vals)
  (if (empty? vals)
      #t
      (let ([v (first vals)]
            [vs (rest vals)])
        (if (empty? vs)
            #t
            (and (check? v (first vs))
                 (check-pairwise check? vs))))))

(define (exists pred . seqs)
  (if (andmap empty? seqs)
      #f
      (or (apply pred (map first seqs))
          (apply exists pred (map rest seqs)))))

(define (for-all pred . seqs)
  (if (andmap empty? seqs)
      #t
      (and (apply pred (map first seqs))
           (apply for-all pred (map rest seqs)))))

(define (kwhash->altlist v)
  (foldr (λ (a b)
           (list* (car a) (cdr a) b))
         null
         (sort (hash->list v)
               (λ (a b)
                 (keyword<? (car a) (car b))))))

(module+ test
  (test-case
      "kwhash->altlist"
    (check-equal? (kwhash->altlist (hash '#:c 2 '#:a 1 '#:b 3)) '(#:a 1 #:b 3 #:c 2))
    (check-equal? (kwhash->altlist (hash '#:a 1)) '(#:a 1))
    (check-equal? (kwhash->altlist (hash)) '())))

(define (interleave a b)
  (match (list a b)
    [(or (list (d:sequence) _) (list _ (d:sequence))) null]
    [(list (d:sequence v vs ...) (d:sequence w ws ...))
     (list* v w (interleave vs ws))]))

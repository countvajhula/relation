#lang racket/base

(require racket/list
         racket/match
         arguments
         qi)

(provide check-pairwise
         exists
         for-all
         find
         kwhash->altlist
         join-list
         singleton?
         arguments-cons)

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

(define (find pred lst)
  (match lst
    ['() #f]
    [(cons v vs) (or (and (pred v) v)
                     (find pred vs))]))

(define (kwhash->altlist v)
  (foldr (λ (a b)
           (list* (car a) (cdr a) b))
         null
         (sort (hash->list v)
               (λ (a b)
                 (keyword<? (car a) (car b))))))

(define (join-list lst)
  (apply append lst))

(define-flow (singleton? seq)
  ;; cheap check to see if a list is of length 1,
  ;; instead of traversing to compute the length
  (and (not empty?)
       (~> rest empty?)))

(define (arguments-cons v args)
  (make-arguments (cons v (arguments-positional args))
                  (arguments-keyword args)))

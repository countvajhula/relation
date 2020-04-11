#lang racket/base

(module+ test
  (require rackunit
           racket/stream
           (except-in data/collection
                      foldl
                      foldl/steps
                      append)
           relation)

  (check-equal? ((unthunk (λ () 5))) 5)
  (check-equal? ((unthunk (λ () 5)) 1) 5)
  (check-equal? ((unthunk (λ () 5)) 1 2 3) 5)
  (check-equal? ((unthunk (λ (v) v) 5) 1 2 3) 5)
  (check-equal? ((unthunk (λ (a b) (+ a b)) 5 4) 1 2 3) 9)
  (check-equal? (andf 1 2 3) (and 1 2 3))
  (check-equal? (andf 1 #f 3) (and 1 #f 3))
  (check-equal? (andf #f #t) (and #f #t))
  (check-equal? (andf #t #t) (and #t #t))
  (check-equal? (orf 1 2 3) (or 1 2 3))
  (check-equal? (orf 1 #f 3) (or 1 #f 3))
  (check-equal? (orf #f #t) (or #f #t))
  (check-equal? (orf #t #t) (or #t #t))
  (check-equal? ((flip string-append) " " "hello" "my" " " "friend") "hello my friend")
  (check-equal? ((flip$ string-append) "friend" "hello" " " "my" " ") "hello my friend")
  (check-equal? ((flip* string-append) "friend" " " "my" " " "hello") "hello my friend")
  (check-equal? ((make-function add1 add1 +) 3 2) 7)
  (check-equal? ((make-function +) 3 2) 5)
  (check-equal? (first (make-function add1 sub1)) add1)
  (check-equal? (second (make-function add1 sub1)) sub1)
  (check-true (empty? (make-function)))
  (let ([str-append-3 (λ (x y z)
                        (string-append x y z))])
    (check-equal? ((curry str-append-3 "hello") " " "there") "hello there")
    (check-equal? (((curry str-append-3 "hello") " ") "there") "hello there")
    (check-equal? ((curryr str-append-3 "there") "hello" " ") "hello there")
    (check-equal? (((curryr str-append-3 "there") " ") "hello") "hello there"))
  (check-true ((conjoin positive? integer?) 5))
  (check-false ((conjoin positive? integer?) -5))
  (check-false ((conjoin bytes<? bytes=?) #"apple" #"banana"))
  (check-true ((disjoin positive? integer?) 5))
  (check-true ((disjoin positive? integer?) -5))
  (check-true ((disjoin positive? integer?) 5.3))
  (check-false ((disjoin positive? integer?) -5.3))
  (check-true ((disjoin bytes<? bytes=?) #"apple" #"banana")))

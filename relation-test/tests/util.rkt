#lang racket/base

(require rackunit
         rackunit/text-ui
         relation/private/util
         relation/function/type/application-scheme/private/util)

(define tests
  (test-suite
   "Utilities"

   (test-suite
    "kwhash->altlist"
    (check-equal? (kwhash->altlist (hash '#:c 2 '#:a 1 '#:b 3)) '(#:a 1 #:b 3 #:c 2))
    (check-equal? (kwhash->altlist (hash '#:a 1)) '(#:a 1))
    (check-equal? (kwhash->altlist (hash)) '()))

   (test-suite
    "application scheme utilities"

    (test-suite
     "revise-arity"
     (check-equal? (revise-arity 5 3) 2)
     (check-equal? (revise-arity 2 3) -1 "an invalid arity, but not the responsibility of this function to check")
     (check-equal? (revise-arity (arity-at-least 3) 2) (arity-at-least 1))
     (check-equal? (revise-arity (arity-at-least 3) 3) (arity-at-least 0))
     (check-equal? (revise-arity (arity-at-least 3) 4) (arity-at-least 0))
     (check-equal? (revise-arity (list 3 4) 2) (list 1 2))
     (check-equal? (revise-arity (list (arity-at-least 3) 4) 2) (list (arity-at-least 1) 2))
     (check-equal? (revise-arity (list (arity-at-least 3) 4) 4) (list (arity-at-least 0) 0))
     (check-equal? (revise-arity (list 3 5) 4) 1))

    (test-suite
     "list-subtract"
     (check-equal? (list-subtract (list 1 2 3) (list 2)) (list 1 3))
     (check-equal? (list-subtract (list 1 2 2 3) (list 2)) (list 1 3) "removes all instances if found")
     (check-equal? (list-subtract (list 1 2 3) (list 5)) (list 1 2 3))
     (check-equal? (list-subtract (list 1 2 3) null) (list 1 2 3))
     (check-equal? (list-subtract null (list 1 2 3)) null)
     (check-equal? (list-subtract null null) null)))))

(module+ test
  (void
   (run-tests tests)))

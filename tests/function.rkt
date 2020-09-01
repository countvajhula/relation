#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/stream
         racket/bool
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         data/maybe
         (only-in racket/function
                  thunk
                  const
                  (conjoin f:conjoin))
         arguments
         relation
         "private/util.rkt")

(define tests
  (test-suite
   "Tests for functional primitives"

   (check-equal? ((unthunk (λ () 5))) 5)
   (check-equal? ((unthunk (λ () 5)) 1) 5)
   (check-equal? ((unthunk (λ () 5)) 1 2 3) 5)
   (check-equal? ((unthunk (λ (v) v) 5) 1 2 3) 5)
   (check-equal? ((unthunk (λ (a b) (+ a b)) 5 4) 1 2 3) 9)
   (check-equal? ((if-f positive? add1 sub1) 3) 4)
   (check-equal? ((if-f positive? add1 sub1) -3) -4)
   (check-equal? ((if-f < + -) 1 2) 3)
   (check-equal? ((if-f < + -) 2 1) 1)
   (check-true (true.))
   (check-true (true. 1 2 3 4))
   (check-false (false.))
   (check-false (false. 1 2 3 4))
   (check-equal? ((arg 0) 1 2 3 4) 1)
   (check-equal? ((arg 2) 1 2 3 4) 3)
   (check-equal? ((arg 3) 1 2 3 "hi") "hi")
   (check-equal? ((flip string-append) " " "hello" "my" " " "friend") "hello my friend")
   (check-equal? ((flip$ string-append) "friend" "hello" " " "my" " ") "hello my friend")
   (check-equal? ((flip* string-append) "friend" " " "my" " " "hello") "hello my friend")
   (check-equal? (->list ((lift add1) (list 1 2 3))) (list 2 3 4))
   (check-equal? (->list ((lift ->string) (list 1 2 3))) (list "1" "2" "3"))
   (check-equal? ((lift add1) (just 3)) (just 4))
   (check-equal? (pack add1 1 2 3) (list 2 3 4))
   (check-equal? (pack ->string 1) (list "1"))
   (check-equal? (pack ->string) (list))
   (check-equal? ((make-function add1 add1 +) 3 2) 7)
   (check-equal? ((make-function +) 3 2) 5)
   (check-equal? (first (make-function add1 sub1)) add1)
   (check-equal? (second (make-function add1 sub1)) sub1)
   (check-true (empty? (make-function)))
   (let ([str-append-3 (procedure-reduce-arity string-append 3)])
     (check-equal? ((curry str-append-3 "hello") " " "there") "hello there")
     (check-equal? (((curry str-append-3 "hello") " ") "there") "hello there")
     (check-equal? ((curry str-append-3 "hello" " ") "there") "hello there")
     (check-equal? ((curryr str-append-3 "there") "hello" " ") "hello there")
     (check-equal? (((curryr str-append-3 "there") " ") "hello") "hello there")
     (check-equal? ((curryr str-append-3 " " "there") "hello") "hello there")
     (check-equal? (length (arguments-positional (function-arguments (((curryr str-append-3 "there") " "))))) 2 "invoking with incomplete args")
     (check-equal? ((function-cons ->bytes (curry str-append-3 "hello" " ")) "there") #"hello there")
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there") "blah" "blah")))
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah") "blah")))
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello") "there" "blah" "blah")))
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah" "blah")))))
   (check-equal? ((curry .. "3") "4") "34")
   (check-equal? ((curry .. "3" "4")) "34")
   (check-equal? ((curryr .. "3") "4") "43")
   (check-equal? ((curryr .. "3" "4")) "34")
   (check-equal? ((curry (f ..) "3") "4") "34")
   (check-equal? ((curry (f ..) "3" "4")) "34")
   (check-equal? ((curryr (f ..) "3") "4") "43")
   (check-equal? ((curryr (f ..) "3" "4")) "34")
   (check-equal? ((curry (curryr .. "3") "4")) "43")
   (check-equal? ((curryr (curryr .. "3") "4")) "43")
   (check-equal? ((curry (curry .. "3") "4")) "34")
   (check-equal? ((curryr (curry .. "3") "4") "5") "354")
   (check-equal? ((curry (curryr .. "3") "4") "5") "453")
   (check-equal? ((curryr (curry power 2) *) 3) 8)
   (check-equal? ((function-cons add1 (f sub1)) 3) 3)
   (check-equal? ((function-cons add1 (function-null)) 3) 4)
   (check-equal? ((function-cons positive? (function-cons integer? (function-null #:compose-with (monoid f:conjoin (const #t))))) 5) #t)
   (check-equal? (->list (apply/steps (f add1 sub1 add1) (list 3))) (list 4 3 4))
   (check-equal? (->list (apply/steps (f ->string sub1 fold) #:into 2 + (list (list 1 2 3 4)))) (list 12 11 "11"))
   (check-equal? ((compose add1 sub1) 3) 3)
   (check-equal? ((compose (f add1) (f sub1)) 3) 3)
   (check-equal? ((compose (f add1) (curry + 2)) 3) 6)
   (check-true ((conjoin positive? integer?) 5))
   (check-false ((conjoin positive? integer?) -5))
   (check-false ((conjoin bytes<? bytes=?) #"apple" #"banana"))
   (check-true ((disjoin positive? integer?) 5))
   (check-true ((disjoin positive? integer?) -5))
   (check-true ((disjoin positive? integer?) 5.3))
   (check-false ((disjoin positive? integer?) -5.3))
   (check-true ((disjoin bytes<? bytes=?) #"apple" #"banana"))
   ;; custom composition
   (check-true ((f #:compose-with (monoid (λ (f g)
                                            (λ (x)
                                              (xor (f x)
                                                   (g x))))
                                          (const #f))
                   positive?
                   integer?
                   (curryr > -3))
                5))
   (check-false ((f #:compose-with (monoid (λ (f g)
                                             (λ (x)
                                               (xor (f x)
                                                    (g x))))
                                           (const #f))
                    positive?
                    integer?
                    (curryr > -3))
                 -1))
   (check-true ((f #:compose-with (monoid (λ (f g)
                                            (λ (x)
                                              (xor (f x)
                                                   (g x))))
                                          (const #f))
                   positive?
                   integer?
                   (curryr > -3))
                -1.4))
   (check-false ((f #:compose-with (monoid (λ (f g)
                                             (λ (x)
                                               (xor (f x)
                                                    (g x))))
                                           (const #f))
                    positive?
                    integer?
                    (curryr > -3))
                 -3.4))
   (check-false ((negate positive?) 5))
   (check-true ((negate positive?) -5))
   (check-true ((negate negative?) 5))
   (check-false ((negate negative?) -5))
   (check-true ((negate positive?) 0))
   (check-true ((negate negative?) 0))))

(module+ test
  (just-do
   (run-tests tests)))

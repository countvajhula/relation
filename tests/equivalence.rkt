#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in b: racket/base)
         racket/set
         racket/stream
         racket/class
         racket/string
         (only-in racket/function
                  identity)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         relation
         "private/util.rkt")

(define tests
  (test-suite
   "Tests for equivalence relations"

   ;; equal / equivalence
   (check-false (= 1 2 3) "monotonically increasing")
   (check-false (= 1 1 2) "monotonically nondecreasing")
   (check-true (= 3 3 3) "equal")
   (check-false (= 3 3 1) "monotonically nonincreasing")
   (check-false (= 3 2 1) "monotonically decreasing")
   (check-false (= 3 2 4) "disordered")
   (check-true (= 3) "trivial case")
   (check-exn exn:fail?
              (lambda ()
                (=)) "generic relations require at least one argument")
   (check-false (= 3 (void)) "non-uniform types")
   (check-false (= "apple" "banana" "cherry"))
   (check-true (= "apple" "apple" "apple"))
   (check-false (= "cherry" "banana" "apple"))
   (check-false (= #"apple" #"banana" #"cherry"))
   (check-true (= #"apple" #"apple" #"apple"))
   (check-false (= #"cherry" #"banana" #"apple"))
   (check-false (= #\a #\b #\c))
   (check-true (= #\a #\a #\a))
   (check-false (= #\c #\b #\a))
   (check-false (= (set) (set 1) (set 1 2)))
   (check-true (= (set 1 2) (set 1 2) (set 1 2)))
   (check-false (= (set 1 2) (set 1) (set)))
   (check-false (= (set 1 2) (set 1 3)) "incomparable sets")
   (check-false (= (set 1 2) (set 3 4)) "incomparable sets")
   (check-true (= (new object%) (new object%)))
   (check-true (= 1 1.0))
   (check-true (= 5/4 1.25))

   ;; simple type-specific equality
   (check-true (= 'hi 'hi 'hi))

   ;; sequences (recursive equality check)
   (check-true (= (list 1 2 3) (list 1 2 3)))
   (check-true (= (list 1.25 2 3) (list 5/4 2 3)) "nested numeric equality")
   (check-true (= (list (list 1.25) 2 3) (list (list 5/4) 2 3)) "nested numeric equality")
   (check-false (= (list 1 2 3) (list 1 2 3 4)) "common prefix")
   (check-false (= "hi" "hide") "common prefix")
   (check-false (= (list 1 2 3) #(1 2 3)) "heterogeneous types")
   (check-false (= (list 1 2 3) (stream 1 2 3)) "heterogeneous types")
   (check-false (= (list (list 1 2 3)) (list (stream 1 2 3))) "nested heterogeneous types")
   (check-false (= (list (list (list 1 2 3))) (list (list (stream 1 2 3)))) "nested heterogeneous types")

   ;; custom types
   ((λ ()
      (struct amount (dollars cents)
        #:transparent
        #:methods gen:comparable
        [(define (equal? comparable other)
           (= (amount-dollars comparable)
              (amount-dollars other)))])
      (check-true (= (amount 5 95) (amount 5 99)))
      (check-false (= (amount 5 95) (amount 4 95)))))

   ;; hash codes
   (check-equal? (hash-code 1) (hash-code 1.0))
   (check-equal? (hash-code 5/4) (hash-code 1.25))
   (check-equal? (hash-code (list 1 2 3)) (hash-code (list 1 2 3)))
   (check-equal? (hash-code 'abc) (hash-code (string->symbol "abc")))
   (check-not-equal? (hash-code (list 1 2 3)) (hash-code (list 2 1 3)))
   (check-not-equal? (hash-code (list 1 2 3)) (hash-code #(1 2 3)))
   (check-equal? (hash-code (list (list 1) 2 3)) (hash-code (list (list 1.0) 2 3)))
   (check-equal? (hash-code (list 1.25 2 3)) (hash-code (list 5/4 2 3)) "nested numeric equality")
   (check-equal? (hash-code (list (list 1.25) 2 3)) (hash-code (list (list 5/4) 2 3)) "nested numeric equality")
   (check-not-equal? (hash-code (list 1 2 3)) (hash-code #(1 2 3)) "heterogeneous types")
   (check-not-equal? (hash-code (list 1 2 3)) (hash-code (stream 1 2 3)) "heterogeneous types")
   (check-not-equal? (hash-code (list (list 1 2 3))) (hash-code (list (stream 1 2 3))) "nested heterogeneous types")
   (check-not-equal? (hash-code (list (list (list 1 2 3)))) (hash-code (list (list (stream 1 2 3)))) "nested heterogeneous types")

   ;; equivalence under a mapping
   (check-true (= #:key identity 1 1 1))
   (check-true (= #:key even? 1 13 7))
   (check-true (= #:key even? 2 14 8))
   (check-false (= #:key even? 2 3))
   (check-true (= #:key string-upcase "apple" "Apple" "APPLE"))
   (check-true (= #:key ->number "42" "42.0" "42/1"))
   (check-false (= #:key ->number "42" "42.1"))
   (check-false (= #:key string-length "z" "yy" "xxx"))
   (check-true (= #:key string-length "xxx" "zzz" "yyy"))
   (check-false (= #:key string-length "xxx" "yy" "z"))

   ;; not equal
   (check-true (≠ 1 2 3) "monotonically increasing")
   (check-true (≠ 1 1 2) "monotonically nondecreasing")
   (check-false (≠ 3 3 3) "equal")
   (check-true (≠ 3 3 1) "monotonically nonincreasing")
   (check-true (≠ 3 2 1) "monotonically decreasing")
   (check-true (≠ 3 2 4) "disordered")
   (check-false (≠ 3) "trivial case")
   (check-exn exn:fail?
              (lambda ()
                (≠)) "generic relations require at least one argument")
   (check-true (≠ "apple" "banana" "cherry"))
   (check-false (≠ "apple" "apple" "apple"))
   (check-true (≠ "cherry" "banana" "apple"))
   (check-true (≠ #"apple" #"banana" #"cherry"))
   (check-false (≠ #"apple" #"apple" #"apple"))
   (check-true (≠ #"cherry" #"banana" #"apple"))
   (check-true (≠ #\a #\b #\c))
   (check-false (≠ #\a #\a #\a))
   (check-true (≠ #\c #\b #\a))
   (check-true (≠ (set) (set 1) (set 1 2)))
   (check-false (≠ (set 1 2) (set 1 2) (set 1 2)))
   (check-true (≠ (set 1 2) (set 1) (set)))
   (check-true (≠ (set 1 2) (set 1 3)) "incomparable sets")
   (check-true (≠ (set 1 2) (set 3 4)) "incomparable sets")
   (check-true (≠ #:key string-length "z" "yy" "xxx"))
   (check-false (≠ #:key string-length "xxx" "zzz" "yyy"))
   (check-true (≠ #:key string-length "xxx" "yy" "z"))
   (check-false (/= 2 2))
   (check-true (/= 3 2))

   (check-true (≠ "apple" "Apple"))
   (check-true (≠ 'a 'b))
   (check-false (≠ 'a 'a))

   ;; group-by
   (check-equal? (->set (group-by identity (list 1 2 3))) (->set '((1) (2) (3))) "monotonically increasing")
   (check-equal? (->set (group-by identity (list 1 1 2))) (->set '((1 1) (2))) "monotonically nondecreasing")
   (check-equal? (->set (group-by identity (list 3 3 3))) (->set '((3 3 3))) "equal")
   (check-equal? (->set (group-by identity (list 3 3 1))) (->set '((1) (3 3))) "monotonically nonincreasing")
   (check-equal? (->set (group-by identity (list 3 2 1))) (->set '((1) (2) (3))) "monotonically decreasing")
   (check-equal? (->set (group-by identity (list 3 2 4))) (->set '((2) (3) (4))) "disordered")
   (check-equal? (->set (group-by identity (list 3))) (->set '((3))) "trivial case")
   (check-exn exn:fail?
              (lambda ()
                (group-by)) "generic relations require at least one argument")
   (check-equal? (->set (group-by identity (list "apple" "banana" "cherry"))) (->set '(("apple") ("banana") ("cherry"))))
   (check-equal? (->set (group-by identity (list "apple" "apple" "apple"))) (->set '(("apple" "apple" "apple"))))
   (check-equal? (->set (group-by identity (list "cherry" "banana" "apple"))) (->set '(("apple") ("banana") ("cherry"))))
   (check-equal? (->set (group-by identity (list #"apple" #"banana" #"cherry"))) (->set '((#"apple") (#"banana") (#"cherry"))))
   (check-equal? (->set (group-by identity (list #"apple" #"apple" #"apple"))) (->set '((#"apple" #"apple" #"apple"))))
   (check-equal? (->set (group-by identity (list #"cherry" #"banana" #"apple"))) (->set '((#"apple") (#"banana") (#"cherry"))))
   (check-equal? (->set (group-by identity (list #\a #\b #\c))) (->set '((#\a) (#\b) (#\c))))
   (check-equal? (->set (group-by identity (list #\a #\a #\a))) (->set '((#\a #\a #\a))))
   (check-equal? (->set (group-by identity (list #\c #\b #\a))) (->set '((#\a) (#\b) (#\c))))
   (check-equal? (->set (group-by identity (list (set) (set 1) (set 1 2)))) (set (list (set)) (list (set 1)) (list (set 1 2))))
   (check-equal? (->set (group-by identity (list (set 1 2) (set 1 2) (set 1 2)))) (set (list (set 1 2) (set 1 2) (set 1 2))))
   (check-equal? (->set (group-by identity (list (set 1 2) (set 1) (set)))) (set (list (set)) (list (set 1)) (list (set 1 2))))
   (check-equal? (->set (group-by identity (list (set 1 2) (set 1 3)))) (set (list (set 1 2)) (list (set 1 3))) "incomparable sets")
   (check-equal? (->set (group-by identity (list (set 1 2) (set 3 4)))) (set (list (set 1 2)) (list (set 3 4))) "incomparable sets")
   (check-equal? (->set (group-by string-length (list "z" "yy" "xxx"))) (->set '(("z") ("yy") ("xxx"))))
   (check-equal? (->set (group-by string-length (list "xxx" "zzz" "yyy"))) (->set '(("xxx" "zzz" "yyy"))))
   (check-equal? (->set (group-by string-length (list "xxx" "yy" "z"))) (->set '(("z") ("yy") ("xxx"))))
   (check-equal? (->set (group-by odd? (list 2 3 4 5 7))) (->set '((2 4) (3 5 7))))

   ;; generic-set
   (check-equal? (->set (generic-set 1 2 3)) (set 1 2 3) "monotonically increasing")
   (check-equal? (->set (generic-set 1 1 2)) (set 1 2) "monotonically nondecreasing")
   (check-equal? (->set (generic-set 3 3 3)) (set 3) "equal")
   (check-equal? (->set (generic-set 3 3 1)) (set 1 3) "monotonically nonincreasing")
   (check-equal? (->set (generic-set 3 2 1)) (set 1 2 3) "monotonically decreasing")
   (check-equal? (->set (generic-set 3 2 4)) (set 2 3 4) "disordered")
   (check-equal? (->set (generic-set 3)) (set 3) "trivial case")
   (check-equal? (->set (generic-set)) (set) "empty set OK")
   (check-equal? (->set (generic-set "apple" "banana" "cherry")) (set "apple" "banana" "cherry"))
   (check-equal? (->set (generic-set "apple" "apple" "apple")) (set "apple"))
   (check-equal? (->set (generic-set "cherry" "banana" "apple")) (set "apple" "banana" "cherry"))
   (check-equal? (->set (generic-set #"apple" #"banana" #"cherry")) (set #"apple" #"banana" #"cherry"))
   (check-equal? (->set (generic-set #"apple" #"apple" #"apple")) (set #"apple"))
   (check-equal? (->set (generic-set #"cherry" #"banana" #"apple")) (set #"apple" #"banana" #"cherry"))
   (check-equal? (->set (generic-set #\a #\b #\c)) (set #\a #\b #\c))
   (check-equal? (->set (generic-set #\a #\a #\a)) (set #\a))
   (check-equal? (->set (generic-set #\c #\b #\a)) (set #\a #\b #\c))
   (check-equal? (->set (generic-set (set) (set 1) (set 1 2))) (set (set) (set 1) (set 1 2)))
   (check-equal? (->set (generic-set (set 1 2) (set 1 2) (set 1 2))) (set (set 1 2)))
   (check-equal? (->set (generic-set (set 1 2) (set 1) (set))) (set (set) (set 1) (set 1 2)))
   (check-equal? (->set (generic-set (set 1 2) (set 1 3))) (set (set 1 2) (set 1 3)) "incomparable sets")
   (check-equal? (->set (generic-set (set 1 2) (set 3 4))) (set (set 1 2) (set 3 4)) "incomparable sets")
   (check-equal? (->set (generic-set #:key string-length "z" "yy" "xxx")) (set "z" "yy" "xxx"))
   (check-equal? (->set (generic-set #:key string-length "xxx" "zzz" "yyy")) (set "xxx"))
   (check-equal? (->set (generic-set #:key string-length "xxx" "yy" "z")) (set "z" "yy" "xxx"))
   (check-false (set-member? (generic-set "xxx" "yy" "z") "YY"))
   (check-true (set-member? (generic-set #:key string-upcase "xxx" "yy" "z") "YY"))
   (check-false (set-member? (generic-set) 5))
   (check-true (set=? (set-add (generic-set 1 2 3) 5) (generic-set 1 2 3 5)))
   (check-true (set=? (set-add (generic-set 3) 3) (generic-set 3)))
   (check-true (set=? (set-add (generic-set) 3) (generic-set 3)))
   (check-true (set=? (set-add (generic-set "apple" "banana" "cherry") "durian") (generic-set "apple" "banana" "cherry" "durian")))
   (check-true (set=? (set-add (generic-set "apple" "banana" "cherry") "BANANA") (generic-set "apple" "banana" "cherry" "BANANA")))
   (check-true (set=? (set-add (generic-set #:key string-upcase "apple" "banana" "cherry") "BANANA") (generic-set #:key string-upcase "apple" "banana" "cherry")))
   (check-true (set=? (set-remove (generic-set 1 2 3) 5) (generic-set 1 2 3)))
   (check-true (set=? (set-remove (generic-set 3) 3) (generic-set)))
   (check-true (set=? (set-remove (generic-set 1 2 3) 3) (generic-set 1 2)))
   (check-true (set=? (set-remove (generic-set "apple" "banana" "cherry") "banana") (generic-set "apple" "cherry")))
   (check-true (set=? (set-remove (generic-set "apple" "banana" "cherry") "BANANA") (generic-set "apple" "banana" "cherry")))
   (check-true (set=? (set-remove (generic-set #:key string-upcase "apple" "banana" "cherry") "BANANA") (generic-set #:key string-upcase "apple" "cherry")))
   (check-true (set=? (generic-set #:key string-upcase "apple" "banana" "cherry") (generic-set #:key string-upcase "apple" "BANANA" "cherry")))
   (check-equal? (set-add (generic-set 1 2 3) 5) (generic-set 1 2 3 5))
   (check-equal? (set-add (generic-set #:key string-upcase "apple" "banana" "cherry") "BANANA") (generic-set #:key string-upcase "apple" "banana" "cherry"))

   ;; tail
   (check-equal? (tail 5 (list 1 2 3)) '())
   (check-equal? (tail 5 (list 1 5 3)) (list 5 3))
   (check-equal? (tail 5 (list)) (list))
   (check-equal? (->list (tail 5 (stream 1 5 3))) (list 5 3))
   (check-equal? (tail #:key string-upcase "HI" (list "hi" "there")) (list "hi" "there"))
   (check-exn exn:fail:contract?
              (λ ()
                (tail 5 (set 1 5 3))))
   (check-exn exn:fail:contract?
              (λ ()
                (tail 10 (generic-set #:key (curryr > 4) 1 2 5 4))))

   ;; member?
   (check-false (member? 5 (set 1 2 3)))
   (check-true (member? 5 (set 1 5 3)))
   (check-false (member? 5 (set)))
   (check-true (member? #:key string-upcase "HI" (set "hi" "there")))
   (check-true (member? #:key string-upcase "HI" (list "hi" "there")))
   (check-false (member? 10 (generic-set #:key (curryr > 4) 1 2 3 4)))
   (check-true (member? 10 (generic-set #:key (curryr > 4) 1 2 5 4)))
   (check-false (member? "HI" (generic-set "hi" "there")))
   (check-true (member? "HI" (generic-set #:key string-upcase "hi" "there")))
   (check-true (member? "HI" (generic-set #:key string? "hi" "there")))
   (check-false (member? #:key string? "HI" (generic-set "hi" "there")) "key ignored for generic sets")
   (check-true (member? "to" (generic-set #:key string-length "hi" "there")) "generic set key used as is")

   ;; assoc
   (check-equal? (assoc 'b (list '(a 1) '(b 2) '(c 3))) '(b 2))
   (check-equal? (assoc 'b (stream '(a 1) '(b 2) '(c 3))) '(b 2))
   (check-false (assoc 'd (list '(a 1) '(b 2) '(c 3))))
   (check-equal? (assoc 2 (list '(1 a) '(2 b) '(3 c))) '(2 b))
   (check-equal? (assoc 2 (list '(1 a) '(2.0 b) '(3 c))) '(2.0 b))
   (check-equal? (assoc #:key string-upcase "abc" (list '("PQR" a) '("XYZ" b) '("ABC" c))) '("ABC" c))
   (check-false (assoc 'b (list)))))

(module+ test
  (just-do
   (run-tests tests)))

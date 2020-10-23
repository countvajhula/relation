#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/stream
         racket/bool
         racket/math
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

   (test-case
       "lambda function form"
     (let ([ff (λ/f (a b) (+ a b))])
       ;; positional args
       (check-true (function? ff))
       (check-equal? (ff 3 4) 7))
     (let ([ff (λ/f (a b . args) (first args))])
       ;; rest args
       (check-true (function? ff))
       (check-equal? (ff 3 4 11 6 3) 11))
     (let ([ff (λ/f (a b #:key [key add1] . args)
                    (join (map key (list* a b args))))])
       ;; kw and rest args
       (check-true (function? ff))
       (check-equal? (ff 1 3 5 7) 20)
       (check-equal? (ff #:key ->string 1 3 5 7) "1357")))
   (test-case
       "Define function form"
     (define/f (add-two a b)
       (+ a b))
     (check-true (function? add-two))
     (check-equal? (add-two 1 2) 3)

     (define/f (first-rest a b . args)
       (first args))
     (check-true (function? first-rest))
     (check-equal? (first-rest 1 2 7 6 3) 7)

     (define/f (join-all a b #:key [key add1] . args)
       (join (map key (list* a b args))))
     (check-true (function? join-all))
     (check-equal? (join-all 1 3 5 7) 20)
     (check-equal? (join-all 1 3 #:key ->string 5 7) "1357"))
   (test-case
       "Lightweight lambda syntactic form"
     (check-equal? ((λ. → 19)) 19)
     (check-equal? ((λ. x → (sqr x)) 5) 25)
     (check-equal? ((λ. x y → (+ x y)) 3 5) 8)
     (check-equal? ((λ. x y #:key key → (= #:key key x y)) #:key values 5 "5") #f)
     (check-equal? ((λ. x y #:key [key #f] → (= #:key key x y)) 5 "5") #f)
     (check-equal? ((λ. x y #:key [key ->number] → (= #:key key x y)) 5 "5") #t))
   (test-case
       "unthunk"
     (check-equal? ((unthunk (λ () 5))) 5)
     (check-equal? ((unthunk (λ () 5)) 1) 5)
     (check-equal? ((unthunk (λ () 5)) 1 2 3) 5)
     (check-equal? ((unthunk (λ (v) v) 5) 1 2 3) 5)
     (check-equal? ((unthunk (λ (a b) (+ a b)) 5 4) 1 2 3) 9))
   (test-case
       "if-f"
     (check-equal? ((if-f positive? add1 sub1) 3) 4)
     (check-equal? ((if-f positive? add1 sub1) -3) -4)
     (check-equal? ((if-f < + -) 1 2) 3)
     (check-equal? ((if-f < + -) 2 1) 1))
   (test-case
       "true."
     (check-true (true.))
     (check-true (true. 1 2 3 4)))
   (test-case
       "false."
     (check-false (false.))
     (check-false (false. 1 2 3 4)))
   (test-case
       "arg"
     (check-equal? ((arg 0) 1 2 3 4) 1)
     (check-equal? ((arg 2) 1 2 3 4) 3)
     (check-equal? ((arg 3) 1 2 3 "hi") "hi"))
   (test-case
       "flips"
     (check-equal? ((flip string-append) " " "hello" "my" " " "friend") "hello my friend")
     (check-equal? ((flip$ string-append) "friend" "hello" " " "my" " ") "hello my friend")
     (check-equal? ((flip* string-append) "friend" " " "my" " " "hello") "hello my friend"))
   (test-case
       "lift"
     (check-equal? (->list ((lift add1) (list 1 2 3))) (list 2 3 4))
     (check-equal? (->list ((lift ->string) (list 1 2 3))) (list "1" "2" "3"))
     (check-equal? ((lift add1) (just 3)) (just 4)))
   (test-case
       "pack"
     (check-equal? (pack add1 1 2 3) (list 2 3 4))
     (check-equal? (pack ->string 1) (list "1"))
     (check-equal? (pack ->string) (list)))
   (test-case
       "make-function"
     (check-equal? ((make-function add1 add1 +) 3 2) 7)
     (check-equal? ((make-function +) 3 2) 5)
     (check-equal? (first (make-function add1 sub1)) add1)
     (check-equal? (second (make-function add1 sub1)) sub1)
     (check-true (empty? (make-function)))
     (check-not-exn (thunk ((f))))
     (check-equal? ((f) 1) 1)
     (check-equal? ((conjoin)) #t)
     (check-equal? ((disjoin)) #f))
   (let ([str-append-3 (procedure-reduce-arity string-append 3)])
     (check-equal? ((curry str-append-3 "hello") " " "there") "hello there")
     (check-equal? (((curry str-append-3 "hello") " ") "there") "hello there")
     (check-equal? ((curry str-append-3 "hello" " ") "there") "hello there")
     (check-equal? ((curryr str-append-3 "there") "hello" " ") "hello there")
     (check-equal? (((curryr str-append-3 "there") " ") "hello") "hello there")
     (check-equal? ((curryr str-append-3 " " "there") "hello") "hello there")
     (check-equal? (length (arguments-positional (function-flat-arguments (((curryr str-append-3 "there") " "))))) 2 "invoking with incomplete args")
     (check-equal? ((function-cons ->bytes (curry str-append-3 "hello" " ")) "there") #"hello there")
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there") "blah" "blah")) "invoking with too many args")
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah") "blah")) "invoking with too many args")
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello") "there" "blah" "blah")) "invoking with too many args")
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah" "blah"))))
     (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah" "blah")))) "invoking with too many args")
   (let ([compare (λ/f (x y #:key [key #f])
                       (= #:key key x y))])
     (check-exn exn:fail:contract? (thunk (compare 5 "5" #:key number->string))
                "all arguments are provided but there is a problem in an argument"))
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
   (check-equal? ((curry (curry (curry string-append "1") "2") "3") "4") "1234")
   (check-equal? ((curry (curry (curryr string-append "1") "2") "3") "4") "2341")
   (check-equal? ((curry (curryr (curry string-append "1") "2") "3") "4") "1342")
   (check-equal? ((curry (curryr (curryr string-append "1") "2") "3") "4") "3421")
   (check-equal? ((curryr (curry (curry string-append "1") "2") "3") "4") "1243")
   (check-equal? ((curryr (curry (curryr string-append "1") "2") "3") "4") "2431")
   (check-equal? ((curryr (curryr (curry string-append "1") "2") "3") "4") "1432")
   (check-equal? ((curryr (curryr (curryr string-append "1") "2") "3") "4") "4321")
   (check-equal? ((curryr (curry power 2) *) 3) 8)
   (test-case
       "Currying in the presence of keyword arguments"
     (define in? (curryr member?))
     (check-true (in? 3 (list 1 2 3)))
     (check-false (in? 4 (list 1 2 3)))
     (check-true ((in? (list 1 2 3)) 3))
     (check-true ((in? #:key ->number (list 1 2 3)) "3"))
     (check-exn exn:fail:contract? (thunk ((in? #:key ->number #:dummy 'dummy (list 1 2 3)) "3")))
     (check-exn exn:fail:contract? (thunk ((in? #:key ->number (list 1 2 3)) "3" "4"))))
   (test-case
       "uncurry"
     (define (curried-add-3 x)
       (λ (y)
         (λ (z)
           (+ x y z))))
     (check-equal? ((uncurry curried-add-3) 1 4 7) 12)
     (check-equal? (((uncurry curried-add-3) 1 4) 7) 12)
     (check-equal? ((((uncurry curried-add-3) 1) 4) 7) 12)
     ;; (check-equal? (((uncurry curried-add-3) 1) 4 7) 12)
     (define string-append-3 (procedure-reduce-arity string-append 3))
     (check-equal? ((((uncurry (curry string-append-3)) "a") "b") "c") "abc")
     (check-equal? (((uncurry (curry string-append-3)) "a" "b") "c") "abc")
     (check-equal? ((uncurry (curry string-append-3)) "a" "b" "c") "abc")
     (check-equal? (((uncurry (curry string-append-3)) "a") "b" "c") "abc"))
   (test-case
       "Function with arguments application scheme"
     (check-equal? ((f #:apply-with empty-arguments +) 1 2 3) 6)
     (define string-append-3 (procedure-reduce-arity string-append 3))
     (check-equal? ((f #:apply-with empty-arguments string-append-3) "a" "b" "c") "abc")
     (check-exn exn:fail:contract:arity? (thunk ((f #:apply-with empty-arguments string-append-3) "a" "b"))))
   (test-case
       "partial"
     (check-true (function? (partial + 1 2 3)))
     (check-equal? ((partial + 1 2 3)) 6)
     (check-equal? ((partial + 1 2 3) 4) 10))
   (test-case
       "partial/template"
     (check-true ((partial/template = #:key (just string-upcase) (just "hi") nothing) "HI"))
     (check-true ((partial/template = #:key nothing (just "hi") nothing) #:key string-upcase "HI"))
     (check-equal? ((app +)) (+))
     (check-equal? ((app + _) 1) (+ 1))
     (check-equal? ((app + _ 2) 1) (+ 1 2))
     (check-equal? ((app + _ 2 _) 1 3) (+ 1 2 3))
     (check-exn exn:fail:contract? (thunk ((app +) 1)) "extra args")
     (check-exn exn:fail:contract? (thunk ((app + _))) "not enough args")
     (define string-append-3 (procedure-reduce-arity string-append 3))
     (check-equal? ((app string-append-3 _ "-" _) "a" "b") (string-append-3 "a" "-" "b"))
     (check-exn exn:fail:contract? (thunk ((app string-append-3 _ "-" _) "a" "b" "c")) "extra args")
     (check-exn exn:fail:contract? (thunk ((app string-append-3 _ "-" _) "a")) "not enough args")
     (check-equal? ((app = #:key string-upcase "hi" _) "HI") (= #:key string-upcase "hi" "HI"))
     (check-exn exn:fail:contract? (thunk ((app = #:key string-upcase "hi" _) #:key string-downcase "HI")) "overriding template not allowed")
     (check-equal? ((app = #:key _ _ "hi") #:key string-upcase "HI") (= #:key string-upcase "hi" "HI"))
     (check-exn exn:fail:contract? (thunk ((app = #:key _ _ "hi") "HI")) "missing keyword arg in template"))
   (check-equal? ((function-cons add1 (f sub1)) 3) 3)
   (check-equal? ((function-cons add1 (function-null)) 3) 4)
   (check-equal? ((function-cons positive? (function-cons integer? (function-null #:compose-with (monoid f:conjoin (const #t))))) 5) #t)
   (check-equal? (function-flat-arguments (curry + 1 2 3)) (make-arguments (list 1 2 3) (hash)))
   (check-equal? (function-flat-arguments (curry = #:key string-upcase "apple")) (make-arguments (list "apple") (hash '#:key string-upcase)))
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

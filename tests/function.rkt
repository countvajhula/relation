#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/stream
         racket/bool
         racket/math
         racket/match
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
         syntax/on
         "private/util.rkt")

(define-predicate (~empty-application? applier)
  (.. (equal? empty-arguments) flat-arguments))

(define-predicate (singleton? seq)
  ;; cheap check to see if a list is of length 1,
  ;; instead of traversing to compute the length
  (and (not empty?)
       (.. empty? rest)))

(define (~maybe-unwrap g)
  ;; if the application is empty
  ;; unwrap atomic function
  ;; composed function if it's a singleton
  ;; and power function if the exponent is 1
  (switch (g)
          [(and function?
                (.. ~empty-application? function-applier))
           (switch (g)
                   [atomic-function? (call atomic-function-f)]
                   [(and composed-function?
                         (.. singleton?
                             composed-function-components))
                    (call (.. first composed-function-components))]
                   [(and power-function? (.. (= 1) power-function-n))
                    (call power-function-f)]
                   [else g])]
          [else g]))

(define (check-naive-composition g0 g1 g)
  (check-equal? (first g) g0)
  (check-equal? (second g) g1)
  (check-equal? (base-composed-function-composer g) usual-composition)
  (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))

(define (check-naive-unwrapped-composition g0 g1 g)
  (check-equal? (first g)
                (~maybe-unwrap g0))
  (check-equal? (second g)
                (~maybe-unwrap g1))
  (check-equal? (base-composed-function-composer g) usual-composition)
  (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))

(define (check-partially-unwrapped-composition g0 g1 g)
  ;; only unwraps the compatible part
  (switch (g0)
          [(and base-composed-function?
                (with-key base-composed-function-composer
                  (eq? (base-composed-function-composer g))))
           (check-equal? (~function-members g0) (take (length g0) g))]
          [else (check-equal? (first g) g0)])
  (switch (g1)
          [(and base-composed-function?
                (with-key base-composed-function-composer
                  (eq? (base-composed-function-composer g))))
           (check-equal? (->list (reverse (~function-members g1))) (->list (take (length g1) (reverse g))))]
          [else (check-equal? (second g) g1)])
  (check-equal? (base-composed-function-composer g) usual-composition)
  (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))

(define-switch (~function-members g)
  [atomic-function? (call (.. list atomic-function-f))]
  [composed-function? (call composed-function-components)]
  [else (call list)])

(define-switch (~underlying-function v)
  [power-function? (call power-function-f)]
  [atomic-function? (call atomic-function-f)]
  [(and composed-function?
        (.. singleton?
            composed-function-components))
   (call (.. first composed-function-components))]
  [else v])

(define (check-unwrapped-composition g0 g1 g)
  ;; note that power is not unwrapped
  (check-equal? (composed-function-components g)
                (append (~function-members g0)
                        (~function-members g1)))
  (check-equal? (base-composed-function-composer g) usual-composition)
  (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))

(define (check-power-composition g0 g1 g)
  (check-true (power-function? g))
  (check-equal? (~underlying-function g) (~underlying-function g0))
  (check-equal? (power-function-n g)
                (+ (if (power-function? g0) (power-function-n g0) 1)
                   (if (power-function? g1) (power-function-n g1) 1)))
  (check-equal? (base-composed-function-composer g) usual-composition)
  (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))

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
       "give"
     (check-equal? (give (curry apply +) 1 2 3) 6)
     (check-equal? (give length "hello" 23 'banana) 3))
   (test-case
       "unwrap"
     (check-equal? (unwrap (list 5)) 5)
     (let-values ([(a b) (unwrap (list 2 3))])
       (check-equal? (list a b) (list 2 3))))
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
   (test-case
       "currying"
     (let ([str-append-3 (procedure-reduce-arity string-append 3)])
       (check-equal? ((curry str-append-3 "hello") " " "there") "hello there")
       (check-equal? (((curry str-append-3 "hello") " ") "there") "hello there")
       (check-equal? ((curry str-append-3 "hello" " ") "there") "hello there")
       (check-equal? ((curryr str-append-3 "there") "hello" " ") "hello there")
       (check-equal? (((curryr str-append-3 "there") " ") "hello") "hello there")
       (check-equal? ((curryr str-append-3 " " "there") "hello") "hello there")
       (check-equal? (length (arguments-positional (function-flat-arguments (((curryr str-append-3 "there") " "))))) 2 "invoking with incomplete args")
       (check-equal? ((function-cons ->bytes (make-composed-function (curry str-append-3 "hello" " "))) "there") #"hello there")
       (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there") "blah" "blah")) "invoking with too many args")
       (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah") "blah")) "invoking with too many args")
       (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello") "there" "blah" "blah")) "invoking with too many args")
       (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah" "blah"))))
       (check-exn exn:fail:contract:arity? (thunk ((curry str-append-3 "hello" "there" "blah" "blah"))) "invoking with too many args"))
     (let ([compare (λ/f (x y #:key [key #f])
                         (= #:key key x y))])
       (check-exn exn:fail:contract? (thunk (compare 5 "5" #:key number->string))
                  "all arguments are provided but there is a problem in an argument"))
     (check-equal? ((curry .. "3") "4") "34")
     (check-equal? ((curry .. "3" "4")) "34")
     (check-equal? ((curryr .. "3") "4") "43")
     (check-equal? ((curryr .. "3" "4")) "34"))
   (test-case
       "left and right currying used together"
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
     (check-equal? ((curryr (curry power 2) *) 3) 8))
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
     (check-exn exn:fail:contract? (thunk ((app = #:key _ _ "hi") "HI")) "missing keyword arg in template")
     (check-equal? ((f string-append #:apply-with (template-arguments 'left (list nothing (just "-") nothing) (hash))) "a" "b") "a-b" "left-chiral template")
     (check-equal? ((f string-append #:apply-with (template-arguments 'right (list nothing (just "-") nothing) (hash))) "a" "b") "b-a") "right-chiral template")
   (test-case
       "application scheme composition"
     (check-equal? ((curry (app string-append _ "-" _) "a") "b") "a-b")
     (check-equal? ((curryr (app string-append _ "-" _) "a") "b") "b-a")
     (check-equal? ((app (app string-append _ "b" _ _) _ "c" "d") "a") "abcd" "nested templates")
     (check-equal? (((curry (app (app string-append _ "b" _ _) _ "c" _)) "a") "d") "abcd" "nested templates and currying")
     (check-equal? (((curryr (app (app string-append _ "b" _ _) _ "c" _)) "d") "a") "abcd" "nested templates and currying")
     (check-exn exn:fail:contract? (thunk ((curry (app string-append _ "-" _) "a") "b" "c")))
     (check-equal? ((partial (app string-append _ "-" _) "a") "b") "a-b")
     (check-exn exn:fail:contract? (thunk (((partial (app string-append _ "-" _)) "a") "b")) "partial application does not curry"))
   (test-case
       "elementary constructors"
     (check-equal? ((function-cons add1 (make-composed-function sub1)) 3) 3)
     (check-equal? ((function-cons add1 (function-null)) 3) 4)
     (check-equal? ((function-cons positive? (function-cons integer? (function-null #:compose-with (monoid f:conjoin (const #t))))) 5) #t))
   (test-case
       "function-flat-arguments"
     (check-equal? (function-flat-arguments (curry + 1 2 3)) (make-arguments (list 1 2 3) (hash)))
     (check-equal? (function-flat-arguments (curry = #:key string-upcase "apple")) (make-arguments (list "apple") (hash '#:key string-upcase))))
   (test-case
       "apply/steps"
     (check-equal? (->list (apply/steps (f add1 sub1 add1) (list 3))) (list 4 3 4))
     (check-equal? (->list (apply/steps (f ->string sub1 fold) #:into 2 + (list (list 1 2 3 4)))) (list 12 11 "11")))
   (test-case
       "compose"
     (check-equal? ((compose add1 sub1) 3) 3)
     (check-equal? ((compose (f add1) (f sub1)) 3) 3)
     (check-equal? ((compose (f add1) (curry + 2)) 3) 6))
   (test-case
       "heterogeneous composition"
     (test-case "non-empty application always composes naively"
       (define test-spec
         (list
          (list add1
                (make-atomic-function + #:apply-with (arguments 1))
                check-naive-composition)
          (list add1
                (make-composed-function sub1 + #:apply-with (arguments 1))
                check-naive-composition)
          (list add1
                (make-power-function add1 3 #:apply-with (arguments 1))
                check-naive-composition)
          (list (make-atomic-function
                 + #:apply-with (pass empty-right-curried-arguments
                                      (arguments 1)
                                      'right))
                (make-atomic-function +)
                check-naive-unwrapped-composition)
          (list (make-atomic-function +)
                (make-atomic-function + #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                check-naive-unwrapped-composition)
          (list (make-composed-function add1 +)
                (make-atomic-function + #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                check-naive-composition)
          (list (make-atomic-function +)
                (make-composed-function add1 +
                                        #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                check-naive-unwrapped-composition)
          (list (make-composed-function add1 +)
                (make-composed-function add1 +
                                        #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                check-naive-composition)
          (list (make-composed-function sub1 +
                                        #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                (make-composed-function add1 +)
                check-naive-composition)
          (list (make-composed-function sub1 +
                                        #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                (make-power-function add1 3)
                check-naive-composition)
          (list (make-power-function add1 3)
                (make-composed-function add1
                                        #:apply-with (pass empty-right-curried-arguments
                                                           (arguments 1)
                                                           'right))
                check-naive-composition)
          (list (make-power-function add1 3)
                (make-power-function add1 2
                                     #:apply-with (pass empty-right-curried-arguments
                                                        (arguments 1)
                                                        'right))
                check-naive-composition)
          (list (make-power-function add1 3)
                (make-atomic-function add1
                                      #:apply-with (pass empty-right-curried-arguments
                                                         (arguments 1)
                                                         'right))
                check-naive-composition)
          (list (make-power-function add1 3
                                     #:apply-with (pass empty-right-curried-arguments
                                                        (arguments 1)
                                                        'right))
                (make-atomic-function add1
                                      #:apply-with (pass empty-right-curried-arguments
                                                         (arguments 1)
                                                         'right))
                check-naive-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "primitive procedures compose naively or as powers"
       (define test-spec
         (list
          (list add1
                sub1
                check-naive-composition)
          (list add1
                +
                check-naive-composition)
          (list add1
                add1
                check-power-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "rich type composed with primitive procedure"
       (define test-spec
         (list
          (list add1
                (make-atomic-function sub1)
                check-naive-unwrapped-composition)
          (list (make-atomic-function sub1)
                add1
                check-naive-unwrapped-composition)
          (list add1
                (make-composed-function sub1 add1)
                check-unwrapped-composition)
          (list (make-composed-function sub1 add1)
                add1
                check-unwrapped-composition)
          (list add1
                (make-power-function add1 2)
                check-power-composition)
          (list (make-power-function add1 2)
                add1
                check-power-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "composing atomic functions"
       (define test-spec
         (list
          (list (make-atomic-function add1)
                (make-atomic-function sub1)
                check-unwrapped-composition)
          (list (make-atomic-function add1)
                (make-atomic-function add1)
                check-power-composition)
          (list (make-atomic-function add1)
                (make-composed-function sub1)
                check-unwrapped-composition)
          (list (make-atomic-function add1)
                (make-composed-function add1)
                check-power-composition)
          (list (make-atomic-function add1)
                (make-power-function sub1 2)
                check-naive-unwrapped-composition)
          (list (make-atomic-function add1)
                (make-power-function add1 2)
                check-power-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "composing compatible compositions"
       (define test-spec
         (list
          (list (make-composed-function add1)
                (make-composed-function sub1)
                check-unwrapped-composition)
          (list (make-composed-function add1)
                (make-composed-function add1)
                check-power-composition)
          (list (make-composed-function add1)
                (make-power-function sub1 2)
                check-unwrapped-composition)
          (list (make-power-function sub1 2)
                (make-composed-function add1)
                check-unwrapped-composition)
          (list (make-composed-function add1)
                (make-power-function add1 2)
                check-power-composition)
          (list (make-power-function add1 2)
                (make-composed-function add1)
                check-power-composition)
          (list (make-power-function add1 2)
                (make-power-function add1 1)
                check-power-composition)
          (list (make-power-function sub1 2)
                (make-power-function add1 1)
                check-naive-unwrapped-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "incompatible higher-level composition"
       (define test-spec
         (list
          (list (make-composed-function add1 #:compose-with conjoin-composition)
                (make-composed-function sub1)
                check-partially-unwrapped-composition)
          (list (make-composed-function add1 #:compose-with conjoin-composition)
                (make-composed-function add1)
                check-partially-unwrapped-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (test-case "incompatible same-level composition"
       (define test-spec
         (list
          (list (make-composed-function add1 #:compose-with conjoin-composition)
                (make-composed-function sub1 #:compose-with disjoin-composition)
                check-naive-composition)
          (list (make-composed-function add1 #:compose-with conjoin-composition)
                (make-composed-function add1 #:compose-with disjoin-composition)
                check-naive-composition)))

       (for-each (λ (spec)
                   (match spec
                     [(list g0 g1 check-fn)
                      (check-fn g0 g1 (compose g0 g1))]))
                 test-spec))
     (let ([g (compose add1 +)])
       ;; built-in procedures
       (check-equal? (first g) add1)
       (check-equal? (second g) +)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments "uses default"))
     (let ([g (compose (make-atomic-function add1)
                       (make-atomic-function + #:apply-with empty-right-curried-arguments))])
       ;; atomic functions
       (check-equal? (first g) add1)
       (check-equal? (second g) +)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments "uses the default"))
     (let ([g (compose (make-composed-function add1 sub1)
                       (make-atomic-function + #:apply-with empty-right-curried-arguments))])
       ;; composed and atomic
       (check-equal? (first g) add1)
       (check-equal? (second g) sub1)
       (check-equal? (third g) +)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments))
     (let ([g (compose (make-atomic-function +)
                       (make-composed-function add1 sub1 #:apply-with empty-right-curried-arguments))])
       ;; atomic and composed
       (check-equal? (first g) +)
       (check-equal? (second g) add1)
       (check-equal? (third g) sub1)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments))
     (let* ([g0 (make-atomic-function +)]
            [g1 (make-composed-function add1
                                        sub1
                                        #:apply-with empty-right-curried-arguments
                                        #:compose-with conjoin-composition)]
            [g (compose g0 g1)])
       ;; atomic and composed with some composer
       (check-equal? (first g) +)
       (check-equal? (second g) g1)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments))
     (let ([g (compose (make-composed-function add1 sub1)
                       (make-composed-function + #:apply-with empty-right-curried-arguments))])
       ;; composed with consonant composition
       (check-equal? (first g) add1)
       (check-equal? (second g) sub1)
       (check-equal? (third g) +)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments))
     (let* ([g0 (make-composed-function add1 sub1
                                        #:compose-with disjoin-composition)]
            [g1 (make-composed-function +
                                        #:compose-with conjoin-composition
                                        #:apply-with empty-right-curried-arguments)]
            [g (compose g0 g1)])
       ;; composed with dissonant composition
       (check-equal? (first g) g0)
       (check-equal? (second g) g1)
       (check-equal? (base-composed-function-composer g) usual-composition)
       (check-equal? (function-applier g) empty-left-curried-arguments "uses default")))
   (test-case
       "conjoin"
     (check-true ((conjoin positive? integer?) 5))
     (check-false ((conjoin positive? integer?) -5))
     (check-false ((conjoin bytes<? bytes=?) #"apple" #"banana")))
   (test-case
       "disjoin"
     (check-true ((disjoin positive? integer?) 5))
     (check-true ((disjoin positive? integer?) -5))
     (check-true ((disjoin positive? integer?) 5.3))
     (check-false ((disjoin positive? integer?) -5.3))
     (check-true ((disjoin bytes<? bytes=?) #"apple" #"banana")))
   (test-case
       "custom composition"
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
                   -3.4)))
   (test-case
       "negate"
     (check-false ((negate positive?) 5))
     (check-true ((negate positive?) -5))
     (check-true ((negate negative?) 5))
     (check-false ((negate negative?) -5))
     (check-true ((negate positive?) 0))
     (check-true ((negate negative?) 0)))))

(module+ test
  (just-do
   (run-tests tests)))

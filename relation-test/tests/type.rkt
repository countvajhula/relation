#lang racket/base

(require rackunit
         rackunit/text-ui
         relation
         (prefix-in b: racket/base)
         racket/set
         racket/math
         racket/stream
         racket/sequence
         racket/generic
         (only-in racket/function
                  identity
                  thunk)
         (only-in data/collection
                  gen:sequence
                  empty?
                  first
                  rest))

(define tests
  (test-suite
   "Tests for type constructors and transformers"

   (test-suite
    "make"
    (check-equal? (make) ID)
    (check-equal? (make null) null)
    (check-equal? (make null 1) '(1))
    (check-equal? (make null 1 2 3) '(3 2 1))
    (check-exn exn:fail:contract? (thunk (make 1 2)))
    (check-equal? (:) ID)
    (check-equal? (: (:)) ID)
    (check-equal? (: ID) ID)
    (check-equal? (: 1 ID) '(1))
    (check-equal? (: 1) '(1))
    (check-equal? (: 1 2 3 ID) '(1 2 3))
    (check-equal? (: 3 null) '(3))
    (check-equal? (: 3 4) '(3 . 4))
    (check-equal? (: 3 (list 1 2)) '(3 1 2))
    (check-equal? (: 3 #(1 2)) #(1 2 3))
    (check-equal? (: '(a . 1) '(d . 4) (hash 'b 2 'c 3)) (hash 'a 1 'b 2 'c 3 'd 4))
    (check-equal? (: 1 2 3 (list 4 5 6)) '(1 2 3 4 5 6))
    (check-equal? (composed-function-components (: ->string sqr (make-composed-function add1))) (list ->string sqr add1))
    (let ([s (set 1 2 3)])
      (:! 2 3 4 s)
      (check-equal? s (set 1 2 3 4))))

   (test-suite
    "->boolean"
    (check-true (->boolean 0))
    (check-true (->boolean (list 1 2)))
    (check-true (->boolean '()))
    (check-true (->boolean ""))
    (check-false (->boolean #f)))

   (test-suite
    "->string"
    (check-equal? (->string 123) "123")
    (check-equal? (->string '(#\a #\p #\p #\l #\e)) "apple")
    (check-equal? (->string '(1 2 3)) "(1 2 3)")
    (check-equal? (->string '()) "")
    (check-equal? (->string ID) ""))

   (test-suite
    "->number"
    (check-equal? (->number "123") 123)
    (check-equal? (->number #\a) 97)
    (check-equal? (->number ID) 0)
    (check-exn exn:fail?
               (lambda ()
                 (->number 'hi)))
    (check-exn exn:fail?
               (lambda ()
                 (->number '(1 2 3)))))

   (test-suite
    "->inexact"
    (check-equal? (->inexact 3/2) 1.5)
    (check-equal? (->inexact 3) 3.0)
    (check-exn exn:fail?
               (lambda ()
                 (->inexact 'hi)))
    (check-exn exn:fail?
               (lambda ()
                 (->inexact '(1 2 3)))))

   (test-suite
    "->exact"
    (check-equal? (->exact 1.5) 3/2)
    (check-equal? (->exact 1.0) 1)
    (check-equal? (->exact ID) 0)
    (check-exn exn:fail?
               (lambda ()
                 (->exact 'hi)))
    (check-exn exn:fail?
               (lambda ()
                 (->exact '(1 2 3)))))

   (test-suite
    "->integer"
    (check-equal? (->integer 1.5) 1)
    (check-equal? (->integer 1.3 #:round 'up) 2)
    (check-equal? (->integer 1.6 #:round 'down) 1)
    (check-equal? (->integer 1.6 #:round 'nearest) 2)
    (check-equal? (->integer #\a) 97)
    (check-equal? (->integer "123") 123)
    (check-equal? (->integer ID) 0)
    (check-exn exn:fail?
               (lambda ()
                 (->integer 'hi)))
    (check-exn exn:fail?
               (lambda ()
                 (->integer '(1 2 3)))))

   (test-suite
    "->list"
    (check-equal? (->list "abc") (list #\a #\b #\c))
    (check-equal? (->list #(1 2 3)) (list 1 2 3))
    (check-equal? (->list (hash 'a 1)) (list (cons 'a 1)))
    (check-equal? (->list (set 'a)) (list 'a))
    (check-equal? (->list ((λ ()
                             (struct amount (dollars cents) #:transparent)
                             (amount 5 95))))
                  '(5 95))
    (check-equal? (->list ID) '())
    (check-exn exn:fail?
               (lambda ()
                 (->list eval))))

   (test-suite
    "->vector"
    (check-equal? (->vector (list 1 2 3)) #(1 2 3))
    (check-equal? (->vector ((λ ()
                               (struct amount (dollars cents) #:transparent)
                               (amount 5 95))))
                  #(5 95))
    (check-equal? (->vector ((λ ()
                               (struct test-seq (seq) #:transparent
                                 #:methods gen:sequence
                                 [(define/generic -empty? empty?)
                                  (define/generic -first first)
                                  (define/generic -rest rest)
                                  (define (empty? this)
                                    (-empty? (test-seq-seq this)))
                                  (define (first this)
                                    (-first (test-seq-seq this)))
                                  (define (rest this)
                                    (-rest (test-seq-seq this)))])
                               (test-seq (list 1 2 3)))))
                  #(1 2 3))
    (check-equal? (->vector "abc") #(#\a #\b #\c))
    (check-equal? (->vector ID) #())
    (check-exn exn:fail?
               (lambda ()
                 (->vector eval))))

   (test-suite
    "->symbol"
    (check-equal? (->symbol "abc") 'abc))

   (test-suite
    "->keyword"
    (check-equal? (->keyword "abc") '#:abc))

   (test-suite
    "->bytes"
    (check-equal? (->bytes (list 97 98 99)) #"abc")
    (check-equal? (->bytes "abc") #"abc")
    (check-equal? (->bytes ID) #"")
    (check-exn exn:fail?
               (lambda ()
                 (->bytes "λ"))))

   (test-suite
    "->char"
    (check-equal? (->char 97) #\a)
    (check-equal? (->char "a") #\a)
    (check-equal? (->char '("a")) #\a)
    (check-equal? (->char 'a) #\a)
    (check-exn exn:fail?
               (lambda ()
                 (->char '(1 2 3)))))

   (test-suite
    "->stream"
    (check-equal? (stream-first (->stream (list 1 2 3))) 1)
    (check-equal? (stream-first (->stream "apple")) #\a)
    (check-true (stream-empty? (->stream ID)))
    (check-exn exn:fail?
               (lambda ()
                 (->stream 'hi))))

   (test-suite
    "->generator"
    (check-equal? ((->generator (list 1 2 3))) 1)
    (check-equal? ((->generator "apple")) #\a)
    (check-equal? (->list (->generator (list 1 2 3))) '(1 2 3))
    (check-equal? (sequence->list (in-producer (->generator (list 1 2 3)) (void))) '(1 2 3))
    (check-equal? (->list (->generator ID)) '()))

   (test-suite
    "->set"
    (check-equal? (set-count (->set (list 1 2 3 1))) 3)
    (check-equal? (set-count (->set "apple")) 4)
    (check-equal? (->set ID) (set))
    (check-exn exn:fail?
               (lambda ()
                 (->set 'hi))))

   (test-suite
    "->symex"
    (check-equal? (->symex (->syntax (list 1 2 3))) '(1 2 3))
    (check-equal? (string->symex "5") 5)
    (check-equal? (string->symex "(+ 1 2)") '(+ 1 2))
    (check-equal? (string->symex "abc") 'abc))

   (test-suite
    "->values"
    (check-equal? (let-values ([(a b c) (->values (list 1 2 3))])
                    (list a b c)) (list 1 2 3))
    (check-exn exn:fail?
               (lambda ()
                 (->values 'hi))))

   (test-suite
    "->hash"
    (check-equal? (->hash ID) (hash))
    (check-equal? (->hash (hash 'a 1)) (hash 'a 1))
    (check-equal? (->hash (list (cons 'a 1) (cons 'b 2))) (hash 'a 1 'b 2))
    (check-exn exn:fail?
               (lambda ()
                 (->hash (list 1 2)))))

   (test-suite
    "->procedure"
    (check-equal? ((->procedure ID) 5) 5)
    (check-equal? (->procedure add1) add1)
    (check-equal? ((->procedure 5)) 5)
    (check-equal? ((->procedure (list 1 2 3)) "blah") (list 1 2 3)))))

(module+ test
  (void
   (run-tests tests)))

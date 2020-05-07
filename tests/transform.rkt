#lang racket/base

(module+ test
  (require rackunit
           relation
           (prefix-in b: racket/base)
           racket/set
           racket/stream
           racket/sequence
           racket/function)

  (check-true (->boolean 0))
  (check-true (->boolean (list 1 2)))
  (check-true (->boolean '()))
  (check-true (->boolean ""))
  (check-false (->boolean #f))

  (check-equal? (->string 123) "123")
  (check-equal? (->string '(#\a #\p #\p #\l #\e)) "apple")
  (check-equal? (->string '(1 2 3)) "(1 2 3)")
  (check-equal? (->string '()) "")
  (check-equal? (->string ID) "")

  (check-equal? (->number "123") 123)
  (check-equal? (->number #\a) 97)
  (check-equal? (->number ID) 0)
  (check-exn exn:fail?
             (lambda ()
               (->number 'hi)))
  (check-exn exn:fail?
             (lambda ()
               (->number '(1 2 3))))

  (check-equal? (->inexact 3/2) 1.5)
  (check-equal? (->inexact 3) 3.0)
  (check-exn exn:fail?
             (lambda ()
               (->inexact 'hi)))
  (check-exn exn:fail?
             (lambda ()
               (->inexact '(1 2 3))))

  (check-equal? (->exact 1.5) 3/2)
  (check-equal? (->exact 1.0) 1)
  (check-equal? (->exact ID) 0)
  (check-exn exn:fail?
             (lambda ()
               (->exact 'hi)))
  (check-exn exn:fail?
             (lambda ()
               (->exact '(1 2 3))))

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
               (->integer '(1 2 3))))

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
               (->list eval)))

  (check-equal? (->vector (list 1 2 3)) #(1 2 3))
  (check-equal? (->vector ((λ ()
                             (struct amount (dollars cents) #:transparent)
                             (amount 5 95))))
                #(5 95))
  (check-equal? (->vector "abc") #(#\a #\b #\c))
  (check-equal? (->vector ID) #())
  (check-exn exn:fail?
             (lambda ()
               (->vector eval)))

  (check-equal? (->symbol "abc") 'abc)

  (check-equal? (->keyword "abc") '#:abc)

  (check-equal? (->bytes (list 97 98 99)) #"abc")
  (check-equal? (->bytes "abc") #"abc")
  (check-equal? (->bytes ID) #"")
  (check-exn exn:fail?
             (lambda ()
               (->bytes "λ")))

  (check-equal? (->char 97) #\a)
  (check-equal? (->char "a") #\a)
  (check-equal? (->char '("a")) #\a)
  (check-equal? (->char 'a) #\a)
  (check-exn exn:fail?
             (lambda ()
               (->char '(1 2 3))))

  (check-equal? (stream-first (->stream (list 1 2 3))) 1)
  (check-equal? (stream-first (->stream "apple")) #\a)
  (check-true (stream-empty? (->stream ID)))
  (check-exn exn:fail?
             (lambda ()
               (->stream 'hi)))

  (check-equal? ((->generator (list 1 2 3))) 1)
  (check-equal? ((->generator "apple")) #\a)
  (check-equal? (->list (->generator (list 1 2 3))) '(1 2 3))
  (check-equal? (sequence->list (in-producer (->generator (list 1 2 3)) (void))) '(1 2 3))
  (check-equal? (->list (->generator ID)) '())

  (check-equal? (set-count (->set (list 1 2 3 1))) 3)
  (check-equal? (set-count (->set "apple")) 4)
  (check-equal? (->set ID) (set))
  (check-exn exn:fail?
             (lambda ()
               (->set 'hi)))

  (check-equal? (->symex (->syntax (list 1 2 3))) '(1 2 3))

  (check-equal? (let-values ([(a b c) (->values (list 1 2 3))])
                  (list a b c)) (list 1 2 3))
  (check-exn exn:fail?
             (lambda ()
               (->values 'hi)))

  (check-equal? (->dict ID) (hash))
  (check-equal? (->dict (hash 'a 1)) (hash 'a 1))
  (check-equal? (->dict (list (cons 'a 1) (cons 'b 2))) (list (cons 'a 1) (cons 'b 2)))
  (check-exn exn:fail?
             (lambda ()
               (->dict (list 1 2))))

  (check-equal? (->procedure ID) identity)
  (check-equal? (->procedure add1) add1)
  (check-equal? ((->procedure 5)) 5)
  (check-equal? ((->procedure (list add1 add1)) 3) 5))

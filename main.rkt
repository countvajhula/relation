#lang racket/base

(module+ test
  (require rackunit)
  (require racket/set)
  (require racket/stream))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require "comparable.rkt"
         "transform.rkt"
         "algebraic.rkt")

(provide (all-from-out "comparable.rkt"
                       "transform.rkt"
                       "algebraic.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  ;; comparable
  (check-true (< 1 2 3) "less than")
  (check-true (<= 1 1 3) "less than or equal to")
  (check-true (= 1 1 1) "equal to")
  (check-true (>= 3 3 1) "greater than or equal to")
  (check-true (> 3 2 1) "greater than")
  (check-false (< 3 2))
  (check-false (<= 3 2))
  (check-false (= 3 2))
  (check-false (>= 2 3))
  (check-false (> 2 3))
  (check-true (≤ 2 3))
  (check-false (≤ 3 2))
  (check-true (≥ 3 2))
  (check-false (≥ 2 3))
  (check-true (< "apple" "banana" "cherry"))
  (check-false (< "banana" "apple" "cherry"))
  (check-true (> "cherry" "banana" "apple"))
  (check-false (> "banana" "apple" "cherry"))
  (check-true (= "banana" "banana"))
  (check-true (= #\a #\a))
  (check-true (< #\a #\b #\c))
  (check-false (< #\b #\a #\c))
  (check-true (> #\c #\b #\a))
  (check-false (> #\b #\c #\a))
  (check-true (< (set) (set 1) (set 1 2)))
  (check-true (<= (set) (set 1) (set 1)))
  (check-true (= (set 1) (set 1) (set 1)))
  (check-true (>= (set 1 2) (set 1 2) (set)))
  (check-true (> (set 1 2) (set 1) (set)))
  (check-false (< (set 1 2) (set 1 2)))
  (check-false (<= (set 1 2) (set)))
  (check-false (= (set 1 2) (set)))
  (check-false (>= (set) (set 1 2)))
  (check-false (> (set 1 2) (set 1 2)))

  ;; transform
  (check-true (->boolean 0))
  (check-false (->boolean #f))
  (check-equal? (->string 123) "123")
  (check-equal? (->number "123") 123)
  (check-equal? (->inexact 3/2) 1.5)
  (check-equal? (->exact 1.5) 3/2)
  (check-equal? (->integer 1.5) 1)
  (check-equal? (->integer 1.3 #:round 'up) 2)
  (check-equal? (->integer 1.6 #:round 'down) 1)
  (check-equal? (->integer 1.6 #:round 'nearest) 2)
  (check-equal? (->list "abc") (list #\a #\b #\c))
  (check-equal? (->list #(1 2 3)) (list 1 2 3))
  (check-equal? (->list (hash 'a 1)) (list (cons 'a 1)))
  (check-equal? (->vector (list 1 2 3)) #(1 2 3))
  (check-equal? (->vector "abc") #(#\a #\b #\c))
  (check-equal? (->symbol "abc") 'abc)
  (check-equal? (->keyword "abc") '#:abc)
  (check-equal? (->bytes (list 97 98 99)) #"abc")
  (check-equal? (->char 97) #\a)
  (check-equal? (stream-first (->stream (list 1 2 3))) 1)
  (check-equal? (set-count (->set (list 1 2 3 1))) 3)
  (check-equal? (->code (->syntax (list 1 2 3))) '(1 2 3))
  (check-equal? (let-values ([(a b c) (->values (list 1 2 3))])
                  (list a b c)) (list 1 2 3)))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))

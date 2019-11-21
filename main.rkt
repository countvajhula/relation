#lang racket/base

(module+ test
  (require rackunit)
  (require racket/set))

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

(require "comparable.rkt")
(provide (all-from-out "comparable.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

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
  (check-false (> (set 1 2) (set 1 2))))

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

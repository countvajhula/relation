#lang info
(define collection "relation")
(define deps '("base"
               "collections-lib"
               "describe"
               "arguments"
               "point-free"
               "threading-lib"
               "mischief"
               "version-case"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "sugar"
                     "collections-doc"
                     "functional-doc"
                     "rackjure"
                     "sandbox-lib"
                     "at-exp-lib"))
(define scribblings '(("scribblings/relation.scrbl" (multi-page))))
(define compile-omit-paths '("dev" "tests"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc" "doc/relation" "tests/compiled"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.8")
(define pkg-authors '(countvajhula))

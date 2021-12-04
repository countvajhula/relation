#lang info
(define collection "relation")
(define deps '("base"
               "collections-lib"
               "describe"
               "functional-lib"
               "arguments"
               "point-free"
               "threading-lib"
               "mischief"
               "social-contract"
               "qi-lib"
               "kw-utils"
               "typed-stack"
               "version-case"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "algebraic"
                     "sugar"
                     "fancy-app"
                     "collections-doc"
                     "functional-doc"
                     "rackjure"
                     "threading-doc"
                     "sandbox-lib"
                     "cover"
                     "cover-coveralls"
                     "at-exp-lib"))
(define scribblings '(("scribblings/relation.scrbl" (multi-page))))
(define compile-omit-paths '("dev" "tests" "coverage"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc" "doc/relation" "tests/compiled" "tests/private/compiled"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.10")
(define pkg-authors '(countvajhula))

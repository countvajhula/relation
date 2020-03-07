#lang info
(define collection "relation")
(define deps '("base"
               "collections-lib"
               "algebraic"
               "lens-lib"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "at-exp-lib"))
(define scribblings '(("scribblings/relation.scrbl" (multi-page))))
(define clean '("compiled" "doc" "doc/relation"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.3")
(define pkg-authors '(countvajhula))

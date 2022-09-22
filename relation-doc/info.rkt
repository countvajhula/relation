#lang info

(define collection "relation")
(define deps '("base"))
(define build-deps '("relation-lib"
                     "scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "arguments"
                     "collections-lib"
                     "functional-lib"
                     "algebraic"
                     "sugar"
                     "fancy-app"
                     "collections-doc"
                     "functional-doc"
                     "rackjure"
                     "threading-doc"
                     "sandbox-lib"))
(define scribblings '(("scribblings/relation.scrbl" (multi-page))))
(define clean '("compiled" "doc" "doc/relation"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.10")
(define pkg-authors '(countvajhula))

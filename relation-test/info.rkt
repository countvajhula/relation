#lang info

(define collection "relation")
(define deps '("base"))
(define build-deps '("arguments"
                     "collections-lib"
                     "functional-lib"
                     "qi-lib"
                     "rackunit-lib"
                     "relation-lib"))
(define clean '("compiled" "tests/compiled" "tests/private/compiled"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.10")
(define pkg-authors '(countvajhula))

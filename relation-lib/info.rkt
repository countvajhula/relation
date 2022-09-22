#lang info

(define collection "relation")
(define deps '("base"
               "collections-lib"
               "describe"
               "functional-lib"
               "arguments"
               "point-free"
               "social-contract"
               "qi-lib"
               "kw-utils"
               "version-case"))
(define build-deps '("at-exp-lib"))
(define clean '("compiled"))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "1.10")
(define pkg-authors '(countvajhula))

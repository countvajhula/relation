#lang info
(define collection "relation")
(define deps '("base" "collections-lib"))
(define build-deps '("scribble-lib" "scribble-abbrevs" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/relation.scrbl" (multi-page))))
(define pkg-desc "Generic interfaces and convenient utilities for relations")
(define version "0.0")
(define pkg-authors '(countvajhula))

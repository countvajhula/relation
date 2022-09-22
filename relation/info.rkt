#lang info

(define collection 'multi)
(define deps '("base"
               "relation-lib"
               "relation-doc"
               "relation-test"))
(define build-deps '())
(define implies '("relation-lib"
                  "relation-doc"
                  "relation-test"))

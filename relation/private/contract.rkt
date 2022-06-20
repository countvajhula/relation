#lang racket/base

(require (except-in racket/contract
                    predicate/c)
         contract/social
         syntax/parse/define
         mischief/shorthand
         version-case
         (for-syntax racket/base))

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(provide variadic-comparison-predicate/c
         variadic-comparison-selection/c)

(define-syntax-parse-rule (variadic-comparison/c type/c return/c)
  ;; TODO: improve to ensure that arguments are type/c
  ;; (rather than any/c) when no key is provided
  (->* (any/c)
       (#:key (maybe/c (encoder/c type/c)))
       #:rest list?
       return/c))

(define-syntax-parse-rule (variadic-comparison-predicate/c type/c)
  (variadic-comparison/c type/c boolean?))

(define-syntax-parse-rule (variadic-comparison-selection/c type/c)
  (variadic-comparison/c type/c any/c))

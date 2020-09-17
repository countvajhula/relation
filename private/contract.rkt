#lang racket/base

(require (except-in racket/contract
                    predicate/c)
         contract/social)

(provide
 (contract-out [variadic-comparison-predicate/c (self-map/c contract?)]
               [variadic-comparison-selection/c (self-map/c contract?)]))

(define (variadic-comparison/c type/c return/c)
  ;; TODO: improve to ensure that arguments are type/c
  ;; (rather than any/c) when no key is provided
  (->* (any/c)
       (#:key (maybe/c (encoder/c type/c)))
       #:rest list?
       return/c))

(define (variadic-comparison-predicate/c type/c)
  (variadic-comparison/c type/c boolean?))

(define (variadic-comparison-selection/c type/c)
  (variadic-comparison/c type/c any/c))

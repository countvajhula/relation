#lang racket/base

(require racket/contract
         (only-in data/collection
                  sequenceof))

(provide perception/c
         optional/c
         variadic-comparison-predicate/c
         variadic-comparison-selection/c
         binary-composition/c
         variadic-composition/c
         reducer/c
         self-map/c
         binary-functional/c
         binary-constructor/c
         variadic-constructor/c)

(define (perception/c as-type)
  (-> any/c as-type))

(define (optional/c contract)
  (or/c contract #f))

(define (variadic-comparison/c type/c return/c)
  ;; TODO: improve to ensure that arguments are type/c
  ;; (rather than any/c) when no key is provided
  (->* (any/c)
       (#:key (optional/c (perception/c type/c)))
       #:rest list?
       return/c))

(define (variadic-comparison-predicate/c type/c)
  (variadic-comparison/c type/c boolean?))

(define (variadic-comparison-selection/c type/c)
  (variadic-comparison/c type/c any/c))

(define (binary-composition/c type/c)
  (-> type/c type/c type/c))

(define (variadic-composition/c type/c)
  (-> type/c ... type/c))

(define (reducer/c type/c)
  (-> (sequenceof type/c) type/c))

(define (self-map/c type/c)
  (-> type/c type/c))

(define binary-functional/c (self-map/c procedure?))

(define (binary-constructor/c primitive/c composite/c)
  (-> primitive/c composite/c composite/c))

(define (variadic-constructor/c primitive/c composite/c)
  (-> primitive/c ... composite/c))

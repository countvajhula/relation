#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/logic
                    (except-in racket = equal? group-by drop length)
                    (only-in racket (= b:=) (equal? b:equal?))
                    (only-in data/collection drop length)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps))
				                 '(require relation)
				                 '(require racket/function)
								 '(require racket/set)
								 '(require racket/stream))))

@title{Logical Relations}

@defmodule[relation/logic]

Logical primitives and predicates.

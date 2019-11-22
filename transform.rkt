#lang racket

(require racket/base)

(provide ->boolean)

(define (->boolean v)
  (if v
      #t
      #f))

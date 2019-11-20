#lang scribble/manual
@require[@for-label[relation
                    racket/base]]

@title{Relation}
@author{Siddhartha Kasivajhula}

@defmodule[relation]

Generic interfaces and convenient utilities for using relations.

@defproc[(< [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically increasing.}

@deftogether[(@defproc[(<= [v comparable?] ...)
              boolean?]
			  @defproc[(≤ [v comparable?] ...)
              boolean?])]{

 True if the v's are monotonically nondecreasing.}

@defproc[(= [v comparable?] ...)
         boolean?]{

 True if the v's are equal.}

@deftogether[(@defproc[(>= [v comparable?] ...)
              boolean?]
			  @defproc[(≥ [v comparable?] ...)
              boolean?])]{

 True if the v's are monotonically nonincreasing.}

@defproc[(> [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically decreasing.}

@defproc[(comparable? [v any/c?])
         boolean?]{

 Predicate to check if a value is comparable via the generic comparison operators @racket[<], @racket[<=], @racket[=], @racket[>=] and @racket[>].}

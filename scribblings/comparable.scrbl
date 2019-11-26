#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
		 racket/sandbox
         @for-label[relation/comparable
		            racket/generic
                    (except-in racket < <= = >= >)]]

@title{Order Relations}

@defmodule[relation/comparable]

A generic interface for comparing data. By default, the built-in comparison operators @racket[<], @racket[<=], @racket[=], @racket[>=] and @racket[>] operate on @tech/reference{numbers} specifically, while other comparable types like characters and strings have their own type-specific comparison operators, for instance @racket[char<?] and @racket[string<?]. This module provides a generic interface that overrides these standard operators to allow their use for any comparable type and not only numbers. You can also provide an implementation for the interface in custom types so that they can be compared using the same standard operators.

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
				                 '(require relation)
								 '(require racket/set))))

@defthing[gen:comparable any/c]{

 A @tech/reference{generic interface} that represents any object that can be compared with other objects of the same type. All built-in types have a default implementation for @racket[gen:comparable], however, only a few of them support comparison relations other than equality. Specifically, the following built-in types have implementations for the order relations @racket[<], @racket[<=], @racket[>=] and @racket[>] in addition to the equality relation @racket[=]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{strings}}
 @item{@tech/reference{characters}}
 @item{@tech/reference{sets}}]

Note that some values may be order-incomparable (see @hyperlink["https://en.wikipedia.org/wiki/Partially_ordered_set"]{partial order}), meaning that none of the order relations would return true for them. For instance, the sets {1, 2} and {1, 3} are incomparable under their canonical order relation (i.e. @racket[subset?]).

@examples[
    #:eval eval-for-docs
    (< 1 2 3)
    (> #\c #\b #\a)
    (< "apple" "banana" "cherry")
    (< (set) (set 1) (set 1 2))
  ]
}

@defproc[(< [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically increasing.

@examples[
    #:eval eval-for-docs
    (< 1 2 3)
    (< 2 1)
    (< "apple" "banana" "cherry")
  ]
}

@deftogether[(@defproc[(<= [v comparable?] ...)
              boolean?]
			  @defproc[(≤ [v comparable?] ...)
              boolean?])]{

 True if the v's are monotonically nondecreasing.

@examples[
    #:eval eval-for-docs
    (≤ 1 1 3)
    (≤ 2 1)
    (≤ "apple" "apple" "cherry")
  ]
}

@defproc[(= [v comparable?] ...)
         boolean?]{

 True if the v's are equal. This uses the most appropriate equality check for the type. For instance, it uses the built-in @racket[=] operator for numeric data, and @racket[equal?] for some other types such as @tech/reference{structures}.

@examples[
    #:eval eval-for-docs
    (= 1 1 1)
    (= 1 2)
    (= "apple" "apple" "apple")
    (= 3/2 1.5)
  ]
}

@deftogether[(@defproc[(>= [v comparable?] ...)
              boolean?]
			  @defproc[(≥ [v comparable?] ...)
              boolean?])]{

 True if the v's are monotonically nonincreasing.

@examples[
    #:eval eval-for-docs
    (≥ 3 1 1)
    (≥ 1 2)
    (≥ "banana" "apple" "apple")
  ]
}

@defproc[(> [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically decreasing.

@examples[
    #:eval eval-for-docs
    (> 3 2 1)
    (> 1 1)
    (> "cherry" "banana" "apple")
  ]
}

@defproc[(comparable? [v any/c])
         boolean?]{

 Predicate to check if a value is comparable via the generic comparison operators @racket[<], @racket[<=], @racket[=], @racket[>=] and @racket[>].

@examples[
    #:eval eval-for-docs
    (comparable? 3)
    (comparable? #\a)
    (comparable? "cherry")
    (comparable? (set))
    (comparable? (hash))
  ]
}
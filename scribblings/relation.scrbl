#lang scribble/doc
@require[scribble/manual
         scribble/eval
         @for-label[relation
		            racket/generic
                    (except-in racket < <= = >= >)]]

@title{Generic Relations}
@author{Siddhartha Kasivajhula}

@defmodule[relation]

A generic interface for comparing data. The built-in Racket operators @racket[<], @racket[<=], @racket[=], @racket[>=] and @racket[>] operate on @tech{numbers} specifically, while other comparable types like characters and strings have their own type-specific comparison operators, for instance @racket[char<?] and @racket[string<?]. This collection provides a generic interface that allows use of the standard operators for any comparable type and not just numbers. The interface can also be implemented in any custom types so that they can be compared using the same operators.

@(define relation-eval (make-base-eval))
@interaction-eval[#:eval relation-eval
                  (require relation)]

@defthing[gen:comparable any/c]{

 A @tech{generic interface} that represents any object that can be compared with other objects of the same type. The following built-in types have implementations for @racket[gen:comparable]:

@itemlist[
 @item{@tech{numbers}}
 @item{@tech{strings}}
 @item{@tech{characters}}]

@examples[
    #:eval relation-eval
    (< 1 2 3)
    (< #\a #\b #\c)
    (< "apple" "banana" "cherry")
  ]
}

@defproc[(< [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically increasing.

@examples[
    #:eval relation-eval
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
    #:eval relation-eval
    (≤ 1 1 3)
    (≤ 2 1)
    (≤ "apple" "apple" "cherry")
  ]
}

@defproc[(= [v comparable?] ...)
         boolean?]{

 True if the v's are equal.

@examples[
    #:eval relation-eval
    (= 1 1 1)
    (= 1 2)
    (= "apple" "apple" "apple")
  ]
}

@deftogether[(@defproc[(>= [v comparable?] ...)
              boolean?]
			  @defproc[(≥ [v comparable?] ...)
              boolean?])]{

 True if the v's are monotonically nonincreasing.

@examples[
    #:eval relation-eval
    (≥ 3 1 1)
    (≥ 1 2)
    (≥ "banana" "apple" "apple")
  ]
}

@defproc[(> [v comparable?] ...)
         boolean?]{

 True if the v's are monotonically decreasing.

@examples[
    #:eval relation-eval
    (> 3 2 1)
    (> 1 1)
    (> "cherry" "banana" "apple")
  ]
}

@defproc[(comparable? [v any/c])
         boolean?]{

 Predicate to check if a value is comparable via the generic comparison operators @racket[<], @racket[<=], @racket[=], @racket[>=] and @racket[>].

@examples[
    #:eval relation-eval
    (comparable? 3)
    (comparable? #\a)
    (comparable? "cherry")
    (comparable? (hash))
  ]
}

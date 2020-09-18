#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
		 racket/sandbox
         @for-label[relation/type
                    (except-in racket < <= = >= >)
					racket/generator]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
				                 '(require relation)
								 '(require racket/set)
								 '(require (only-in data/collection conj))
								 '(require racket/stream))))

@title{Types}

@defmodule[relation/type]

@margin-note{This module was formerly named @racket[relation/transform]. Any code using @racket[relation/transform] directly should be changed to use @racket[relation/type] instead. The former alias is still provided alongside the new one for backwards compatibility, but will be removed in a future version.}

Generic utilities for constructing data and transforming it from one type to another.

The type constructors and transformers provided by Racket out of the box are @italic{type-specific}; for instance in order to construct a list, we use @racket[cons] or @racket[list], and for a stream we'd use @racket[stream-cons] or @racket[stream]. Likewise, to convert data into a string, we would use @racket[symbol->string] if the data is a symbol, and @racket[number->string] if the data is a number. Similarly, converting a number to an integer from a more precise form, or vice versa, typically involves multiple steps and the method varies depending on the number's type.

This module provides a generic type constructor that constructs a type by referring to a provided instance, and also provides convenient interfaces to perform many common type conversions, while keeping them agnostic to the source type. If mutable and immutable versions of a data type exist, these interfaces will return the immutable version.

See also: @other-doc['(lib "sugar/scribblings/sugar.scrbl")].

@section[#:tag "type:interface"]{Interface}

@defthing[gen:form any/c]{

 A @tech/reference{generic interface} that represents a form that an object takes, i.e. its "type." All built-in as well as @seclink["define-struct" #:doc '(lib "scribblings/guide/guide.scrbl")]{custom} types are @racket[gen:form].

}

@defproc[(make [form any/c] [element any/c] ...)
         form?]{

 A function taking a form and any number of elements and constructs a composite form out of them. @racket[form] is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type). The function must return an instance of the structure type.

 Every implementation of @racket[gen:form] must provide an implementation of @racket[make].
}

@defproc[(form? [v any/c])
         boolean?]{

 Predicate to check if a value is a form. In general, primitives in Racket such as numbers and characters are not forms, while composite types like @tech/reference{lists} and @tech/reference{dictionaries} are.

@examples[
    #:eval eval-for-docs
    (form? 3)
    (form? #\a)
    (form? "cherry")
    (form? (set))
    (form? (hash))
    (form? (list))
    (form? (stream))
  ]
}

@section[#:tag "type:constructors"]{Constructors}

@defproc[(: [element any/c] ... [form any/c])
         any/c]{

 A generic constructor that creates a value of the same type as @racket[form] out of the provided @racket[elements]. This utility wraps @racket[make], providing a bit of syntactic sugar. In particular, the order of arguments is reversed to resemble the familiar @racket[cons] list constructor. Additionally, if none of the input values are @racket[form]s, then the @racket[element]s are simply @racket[cons]'d together (if there are two of them) or collected into a @racket[list] (if there are more).

@examples[
    #:eval eval-for-docs
	(: 1 2)
	(: 1 null)
	(: 1 (list 2 3))
	(: 1 2 3 4 (list 5 6))
	(: 1 empty-stream)
	(: 1 #(2 3))
  ]
}

@section[#:tag "type:transformers"]{Transformers}

@defproc[(->boolean [v any/c])
         boolean?]{

 Maps the input data to a @tech/reference{boolean}. Note that in Racket, out of the box, anything that isn't @racket[#f] is treated as @racket[#t], including, for instance, the empty string and the number 0.

@examples[
    #:eval eval-for-docs
    (->boolean 42)
    (->boolean #f)
    (->boolean "apple")
  ]
}

@defproc[(->string [v any/c])
         string?]{

 Maps the input data to a @tech/reference{string}.

@examples[
    #:eval eval-for-docs
    (->string 42)
    (->string 'apple)
    (->string '(1 2 3))
    (->string ID)
  ]
}

@defproc[(->number [v any/c])
         number?]{

 Maps the input data to a @tech/reference{number}.

@examples[
    #:eval eval-for-docs
    (->number "42")
    (->number #\a)
  ]
}

@defproc[(->inexact [v any/c])
         inexact?]{

 Maps the input data to an @tech/reference{inexact number}.

@examples[
    #:eval eval-for-docs
    (->inexact 3/2)
    (->inexact "42")
  ]
}

@defproc[(->exact [v any/c])
         exact?]{

 Maps the input data to an @tech/reference{exact number}.

@examples[
    #:eval eval-for-docs
    (->exact 1.5)
    (->exact "42")
  ]
}

@defproc[(->integer [v any/c] [#:round round (one-of/c 'up 'down 'nearest) 'down])
         integer?]{

 Maps the input data to an @tech/reference{integer}.

@examples[
    #:eval eval-for-docs
    (->integer "42")
    (->integer 3/2)
    (->integer 3/2 #:round 'up)
    (->integer 3.6 #:round 'nearest)
  ]
}

@defproc[(->list [v any/c])
         list?]{

 Maps the input data to a @tech/reference{list}.

@examples[
    #:eval eval-for-docs
    (->list "apple")
    (->list #(1 2 3))
    (->list (stream 1 2 3))
    (->list (hash 'a 1 'b 2 'c 3))
  ]
}

@defproc[(->vector [v any/c])
         vector?]{

 Maps the input data to a @tech/reference{vector}.

@examples[
    #:eval eval-for-docs
    (->vector "apple")
    (->vector '(1 2 3))
    (->vector (stream 1 2 3))
    (->vector (hash 'a 1 'b 2 'c 3))
  ]
}

@defproc[(->symbol [v any/c])
         symbol?]{

 Maps the input data to a @tech/reference{symbol}.

@examples[
    #:eval eval-for-docs
    (->symbol "apple")
    (->symbol '#:apple)
  ]
}

@defproc[(->keyword [v any/c])
         keyword?]{

 Maps the input data to a @tech/reference{keyword}.

@examples[
    #:eval eval-for-docs
    (->keyword "apple")
    (->keyword 'apple)
  ]
}

@defproc[(->bytes [v any/c])
         bytes?]{

 Maps the input data to a @tech/reference{byte string}.

@examples[
    #:eval eval-for-docs
    (->bytes "apple")
    (->bytes '(97 112 112 108 101))
  ]
}

@defproc[(->char [v any/c])
         char?]{

 Maps the input data to a @tech/reference{character}.

@examples[
    #:eval eval-for-docs
    (->char "a")
    (->char 97)
    (->char 'a)
  ]
}

@defproc[(->stream [v any/c])
         stream?]{

 Maps the input data to a @tech/reference{stream}.

@examples[
    #:eval eval-for-docs
    (->stream "apple")
    (->stream '(97 112 112 108 101))
  ]
}

@defproc[(->generator [v any/c] [return any/c (void)])
         generator?]{

 Maps the input data to a @tech/reference{generator}. If a @racket[return] value is provided, it will be used as the return value of the generator once the sequence @racket[v] has been exhausted. Any sequence can be transformed into a generator, and vice versa. This allows us to leverage sequence patterns for generators in a natural way, for instance cons-ing and extending generators to produce additional values by transforming them into streams and then back again.

 Note that, owing to the stateful nature of the underlying generator, it's possible that a stream constructed from a generator would continue to provide lazy evaluation but not take up constant memory. On the other hand, a stream to generator conversion should not incur any additional memory overhead.

 Another thing to be wary of with a generator to stream conversion is that since the underlying generator is mutable, independent invocations of the generator after the stream has been constructed would affect the sequence represented by the stream, which is likely to result in unexpected behavior. In general it is advisable to manipulate stateful entities such as generators via a single common interface, whether that is, in the present case, the generator itself directly, or the stream representation of it -- but not both.

@examples[
    #:eval eval-for-docs
    (->generator "apple")
    (->generator '(97 112 112 108 101))
	(->list (->generator (conj (->stream (->generator '(1 2 3))) 4)))
  ]
}

@defproc[(->set [v any/c])
         set?]{

 Maps the input data to a @tech/reference{set}.

@examples[
    #:eval eval-for-docs
    (->set "apple")
    (->set '(1 2 2 3 3 3))
  ]
}

@defproc[(->syntax [v any/c]
                   [ctx syntax? #f])
         syntax?]{

 Constructs a @tech/reference{syntax object} wrapping the provided @hyperlink["https://en.wikipedia.org/wiki/S-expression"]{symbolic expression} @racket[v]. If the input is already a syntax object, then it is left unchanged. If a syntax object @racket[ctx] is provided, it will be used as the context in constructing the new syntax object.

@examples[
    #:eval eval-for-docs
    (->syntax "apple")
    (->syntax 42)
    (->syntax '(+ 1 2))
  ]
}

@defproc[(->symex [v any/c])
         any/c]{

 Maps a @tech/reference{syntax object} to its underlying @hyperlink["https://en.wikipedia.org/wiki/S-expression"]{symbolic expression}, the literal "code" that will be evaluated by the interpreter. If the input is already a symex, then it is left unchanged.

@examples[
    #:eval eval-for-docs
    (->symex "apple")
    (->symex #'42)
    (->symex #'(+ 1 2))
    (->symex #'(define (square x) (* x x)))
    (eval (->symex #'((λ (x) (* x x)) 4)))
  ]
}

@defproc[(->values [v any/c])
         values?]{

 Maps the input data to a set of @tech/reference{values}.

@examples[
    #:eval eval-for-docs
    (->values #(1 2 3))
    (->values '(1 2 3))
    (->values "apple")
  ]
}

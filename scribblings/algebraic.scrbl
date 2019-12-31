#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
		 racket/sandbox
         @for-label[relation/algebraic
		            racket/generic
                    (except-in racket + - identity)]]

@title{Algebraic Operations}

@defmodule[relation/algebraic]

Generic algebraic operations. The built-in algebraic operators @racket[+] and @racket[*] operate on numbers specifically. Often, however, we are interested in performing operations "similar" to these for datatypes that aren't numbers, for which we would resort to type-specific operators like @racket[append] for lists. This module generalizes the standard algebraic operators to work on any type that supports a "canonical" notion of addition or composition. Specifically, the operator @racket[+] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Group_(mathematics)"]{group} operation (e.g. addition, for numbers), while @racket[..] or @racket[∘] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Monoid"]{monoid} operation (e.g. concatenation, for strings and lists). This allows our intuitions about addition and composition to extend over many types via the generic operators @racket[+] and @racket[..] (or @racket[∘]).

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
				                 '(require relation)
								 '(require racket/set)
								 '(require racket/stream))))

@defthing[gen:monoid any/c]{

 A @tech/reference{generic interface} that represents any object for which a "composition-like" (monoid) operation can be defined. The following built-in types have implementations for @racket[gen:monoid]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{strings}}
 @item{@tech/reference{byte strings}}
 @item{@tech/reference{lists}}
 @item{@tech/reference{vectors}}
 @item{@tech/reference{sets}}
 @item{@tech/reference{sequences}}
 @item{@seclink["procedures" "procedures" #:doc '(lib "scribblings/reference/reference.scrbl")]}
]

@examples[
    #:eval eval-for-docs
    (.. 1 2 3 4)
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
  ]
}

@deftogether[(@defproc[(.. [v monoid?] ...)
              monoid?]
			  @defproc[(∘ [v monoid?] ...)
              monoid?])]{

 Performs the canonical "composition-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic monoid.

@examples[
    #:eval eval-for-docs
    (.. 1 2 3 4)
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
    (.. (hash 'a 1 'b 2) (hash 'c 3))
	((∘ ->string +) 3 4)
  ]
}

@defproc[(identity [v monoid?] [operation procedure?])
         monoid?]{

 Produce the "identity" element for the given value and operation. For numbers and addition, this yields 0, while for numbers and multiplication it yields 1. Likewise, for vector addition this yields the zero vector.

@examples[
    #:eval eval-for-docs
    (identity 3 +)
    (identity 3 *)
    (identity #(1 2 -3) +)
  ]
}

@defproc[(monoid? [v any/c])
         boolean?]{

 Predicate to check if a value may be operated on using the generic composition operator, @racket[..] or @racket[∘].

@examples[
    #:eval eval-for-docs
    (monoid? 3)
    (monoid? #\a)
    (monoid? "cherry")
    (monoid? (list))
    (monoid? (set))
    (monoid? (hash))
    (monoid? (vector))
  ]
}

@defthing[gen:group any/c]{

 A @tech/reference{generic interface} that represents any object for which an "addition-like" (group) operation can be defined. The following built-in types have implementations for @racket[gen:group]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{vectors}}]

@examples[
    #:eval eval-for-docs
    (+ 1 2 3)
    (+ #(1 2 3) #(1 2 3) #(1 2 3))
  ]
}

@defproc[(+ [v group?] ...)
         group?]{

 Performs the canonical "addition-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic group.

@examples[
    #:eval eval-for-docs
    (+ 1 2 3)
    (+ #(1 2 3) #(1 2 3) #(1 2 3))
  ]
}

@defproc[(inverse [v group?] [operation procedure?])
         group?]{

 Produce the "inverse" of the value, based the operation to be performed and the type of the value. For numbers and addition, this yields the number with the opposite sign, while for vectors and addition, this yields the inverse vector.

@examples[
    #:eval eval-for-docs
    (inverse 3 +)
    (inverse 3 *)
    (inverse #(1 2 -3) +)
  ]
}

@defproc[(- [v group?] ...)
         group?]{

 A general version of "subtraction" that works no differently than usual on numbers, but also supports any other group type, for instance, vectors. The result is computed by adding the first supplied value to the @racket[inverse] of every subsequent value. If only one argument is provided, then it simply returns the additive @racket[inverse].

@examples[
    #:eval eval-for-docs
    (- 5 3)
    (- #(3 3 3) #(0 1 0) #(0 0 2))
    (- 5)
  ]
}

@defproc[(group? [v any/c])
         boolean?]{

 Predicate to check if a value may be operated on using the generic addition operator, @racket[+].

@examples[
    #:eval eval-for-docs
    (group? 3)
    (group? #\a)
    (group? "cherry")
    (group? (list))
    (group? (set))
    (group? (hash))
    (group? (vector))
  ]
}

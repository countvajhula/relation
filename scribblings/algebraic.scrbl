#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/algebraic
                    racket/generic
                    (except-in racket +
                                      -
                                      *
                                      /
                                      foldl
                                      foldr
                                      append)
                    (only-in racket (foldl f:foldl)
                                    (foldr f:foldr)
                                    (append b:append))
                    (only-in data/collection (foldl d:foldl)
                                             (foldl/steps d:foldl/steps))]]

@title{Algebraic Operations}

@defmodule[relation/algebraic]

Generic algebraic operations. The built-in algebraic operators @racket[+] and @racket[*] operate on numbers specifically. Often, however, we are interested in performing operations "similar" to these for datatypes that aren't numbers, for which we would resort to type-specific operators like @racketlink[b:append "append"] for lists. This module generalizes the standard algebraic operators to work on any type that supports a "canonical" notion of addition, multiplication, or concatenation. Specifically, the operator @racket[+] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Group_(mathematics)"]{group} operation (e.g. addition, for numbers), while @racket[..] or @racket[∘] performs the canonical @hyperlink["https://en.wikipedia.org/wiki/Monoid"]{monoid} operation (e.g. concatenation, for strings and lists). This allows our intuitions about addition and composition to extend over many types via the generic operators @racket[+] and @racket[..] (or @racket[∘]).

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation)
                                 '(require racket/set)
                                 '(require racket/stream))))

@section[#:tag "algebraic:interfaces"]{Interfaces}

This module provides three generic interfaces -- @racket[gen:appendable], @racket[gen:multipliable], and @racket[gen:addable]. These are meant to represent the canonical "idea" of the operations of concatenation, multiplication and addition, respectively, whose behavior may be customized for each type via these generic interfaces, and used via the common operators @racket[..] (concatenation), @racket[*] (multiplication), and @racket[+] (addition).

@subsection{Concatenation}

@defthing[gen:appendable any/c]{

 A @tech/reference{generic interface} that represents any object for which a concatenation or "append-like" operation can be defined. The following built-in types have implementations for @racket[gen:appendable]:

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
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
  ]

@defproc[(appendable? [v any/c])
         boolean?]{

 Predicate to check if a value may be operated on using the generic append operator, @racket[..] or @racket[∘].

@examples[
    #:eval eval-for-docs
    (appendable? 3)
    (appendable? #\a)
    (appendable? "cherry")
    (appendable? (list))
    (appendable? (set))
    (appendable? (hash))
    (appendable? (vector))
  ]
}

 To implement this interface for custom types, the following methods need to be implemented:

 @defproc[(append [a appendable?]
                  [b appendable?])
          appendable?]{

 A function taking two arguments that composes them in the natural "append-like" operation for the type. Both arguments are expected to be instances of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(appendable-identity [a appendable?])
          appendable?]{

 A function that returns the identity value for the type, for the @racket[append] operation. The identity value is that value which, when composed with other values of the same type under the @racket[append] operation, yield those other values as if there had been no composition. The identity value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(appendable-inverse [a appendable?])
          appendable?]{

 A function that returns the inverse value for the type, for the @racket[append] operation. The inverse value is that value which, when composed with other values of the same type under the @racket[append] operation, yields the identity value. The inverse value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).

 Providing an implementation for this method is optional, and most types would usually not define an inverse for an append-like operation.
 }

}

@subsection{Multiplication}

@defthing[gen:multipliable any/c]{

 A @tech/reference{generic interface} that represents any object for which a "multiplication-like" operation can be defined. The following built-in types have implementations for @racket[gen:appendable]:

@itemlist[
 @item{@tech/reference{numbers}}
]

 Since numbers are the only common type for which the @racket[multiply] operation is defined, this interface is present mostly for uniformity, being leveraged by utilities like @racket[fold], and of course also for use in custom types.

@examples[
    #:eval eval-for-docs
    (* 1 2 3 4)
  ]

@defproc[(multipliable? [v any/c])
         boolean?]{

 Predicate to check if a value may be operated on using the generic multiplication operator, @racket[*].

@examples[
    #:eval eval-for-docs
    (multipliable? 3)
    (multipliable? #\a)
    (multipliable? "cherry")
    (multipliable? (list))
  ]
}

 To implement this interface for custom types, the following methods need to be implemented:

 @defproc[(multiply [a multipliable?]
                    [b multipliable?])
          multipliable?]{

 A function taking two arguments that composes them in the natural "multiplication-like" operation for the type. Both arguments are expected to be instances of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(multipliable-identity [a multipliable?])
          multipliable?]{

 A function that returns the identity value for the type, for the @racket[multiply] operation. The identity value is that value which, when composed with other values of the same type under the @racket[multiply] operation, yield those other values as if there had been no composition. The identity value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(multipliable-inverse [a multipliable?])
          multipliable?]{

 A function that returns the inverse value for the type, for the @racket[multiply] operation. The inverse value is that value which, when composed with other values of the same type under the @racket[multiply] operation, yields the identity value. The inverse value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).

 Providing an implementation for this method is optional; a @racket[multiply] operation need not admit an inverse, for instance, for a custom integer type, multiplication would not define an inverse since multiplicative inverses for numbers would be rational numbers, rather than integers, thus being excluded by the "same type" requirement above.
 }

}

@subsection{Addition}

@defthing[gen:addable any/c]{

 A @tech/reference{generic interface} that represents any object for which an "addition-like" (group) operation can be defined. The following built-in types have implementations for @racket[gen:addable]:

@itemlist[
 @item{@tech/reference{numbers}}
 @item{@tech/reference{vectors}}]

@examples[
    #:eval eval-for-docs
    (+ 1 2 3)
    (+ #(1 2 3) #(1 2 3) #(1 2 3))
  ]

@defproc[(addable? [v any/c])
         boolean?]{

 Predicate to check if a value may be operated on using the generic addition operator, @racket[+].

@examples[
    #:eval eval-for-docs
    (addable? 3)
    (addable? #\a)
    (addable? "cherry")
    (addable? (list))
    (addable? (set))
    (addable? (hash))
    (addable? (vector))
  ]
}

 To implement this interface for custom types, the following methods need to be implemented:

 @defproc[(add [a addable?]
               [b addable?])
          addable?]{

 A function taking two arguments that composes them in the natural "addition-like" operation for the type. Both arguments are expected to be instances of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(addable-identity [a addable?])
          addable?]{

 A function that returns the identity value for the type, for the @racket[add] operation. The identity value is that value which, when composed with other values of the same type under the @racket[add] operation, yield those other values as if there had been no composition. The identity value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(addable-inverse [a addable?])
          addable?]{

 A function that returns the inverse value for the type, for the @racket[add] operation. The inverse value is that value which, when composed with other values of the same type under the @racket[add] operation, yields the identity value. The inverse value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).

 Providing an implementation for this method is required; an addition operation must admit an inverse.
 }

}

@section[#:tag "algebraic:utilities"]{Utilities}

@deftogether[(@defproc[(.. [v appendable?]
                           ...)
                       appendable?]
              @defproc[(∘ [v appendable?]
                          ...)
                       appendable?])]{

 Performs the canonical "append-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic semigroup or monoid.

@examples[
    #:eval eval-for-docs
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
    (.. (hash 'a 1 'b 2) (hash 'c 3))
    ((.. ->string +) 3 4)
    (∘ "hi" " " "there")
  ]
}

@defproc[(* [v multipliable?]
            ...)
         multipliable?]{

 Performs the canonical "multiplication-like" operation on the data, based on its type.

@examples[
    #:eval eval-for-docs
    (* 1 2 3 4)
  ]
}

@defproc[(+ [v addable?]
            ...)
         addable?]{

 Performs the canonical "addition-like" operation on the data, based on its type. This operation is the natural operation on the data type that forms an algebraic group.

@examples[
    #:eval eval-for-docs
    (+ 1 2 3 4)
    (+ #(1 2 3) #(1 2 3) #(1 2 3))
  ]
}

@defproc[(id [operation procedure?])
         procedure?]{

 Produce the "identity" procedure for the given canonical operation. For numbers and addition, this yields 0, while for numbers and multiplication it yields 1. Likewise, for vector addition this yields the zero vector.

@examples[
    #:eval eval-for-docs
    ((id add) 3)
    ((id multiply) 3)
    ((id add) #(1 2 -3))
    ((id append) "hi")
    ((id append) '(1 2 3))
    ((id ..) "hi")
    ((id +) 3)
    ((id *) 3)
  ]
}

@defproc[(inverse [operation procedure?])
         procedure?]{

 Produce the "inverse" of the value, based the operation to be performed and the type of the value. For numbers and addition, this yields the number with the opposite sign, while for vectors and addition, this yields the inverse vector.

@examples[
    #:eval eval-for-docs
    ((inverse +) 3)
    ((inverse *) 3)
    ((inverse +) #(1 2 -3))
  ]
}

@defproc[(- [v addable?]
            ...)
         addable?]{

 A general version of "subtraction" that works no differently than usual on numbers, but also supports any other group type, for instance, vectors. The result is computed by adding the first supplied value to the @racket[inverse] of every subsequent value. If only one argument is provided, then it simply returns the additive @racket[inverse].

@examples[
    #:eval eval-for-docs
    (- 5 3)
    (- #(3 3 3) #(0 1 0) #(0 0 2))
    (- 5)
  ]
}

@defproc[(/ [v multipliable?]
            ...)
         multipliable?]{

 A general version of "division" that works no differently than usual on numbers, but also supports any other @racketlink[gen:multipliable "multipliable"] type. The result is computed by multiplying the first supplied value with the @racket[inverse] of every subsequent value. If only one argument is provided, then it simply returns the multiplicative @racket[inverse].

@examples[
    #:eval eval-for-docs
    (/ 5 3)
    (/ 5)
  ]
}

@deftogether[(@defproc[(fold [f (-> any/c any/c any/c)]
                             [vs (sequenceof any/c)]
                             [base any/c #f]
                             [#:order order (one-of/c 'abb 'bab) 'abb]
                             [#:direction direction (one-of/c 'left 'right) 'right]
                             [#:with-steps with-steps boolean? #f])
                       any/c]
              @defproc[(foldl [f (-> any/c any/c any/c)]
                              [vs (sequenceof any/c)]
                              [base any/c #f]
                              [#:order order (one-of/c 'abb 'bab) 'abb]
                              [#:with-steps with-steps boolean? #f])
              any/c]
              @defproc[(foldr [f (-> any/c any/c any/c)]
                              [vs (sequenceof any/c)]
                              [base any/c #f]
                              [#:order order (one-of/c 'abb 'bab) 'abb]
                              [#:with-steps with-steps boolean? #f])
                       any/c]
              )]{

 Similar to @racketlink[f:foldl "foldl"] and @racketlink[f:foldr "foldr"], but infers the relevant @racketlink[id "identity"] element and uses it as the base value, if none is provided. The identity element is determined by considering the first element of the input sequence together with the given operation.

 With folding operations there are two parameters that one may wish to tweak. The first is the direction of the fold, either left or right, for which one may use either @racket[foldl] or @racket[foldr]. The second is the order in which arguments are supplied to the folding function @racket[f], which may be controlled by the keyword argument @racket[#:order], with a value of @racket['abb] corresponding to the accumulator always being passed second, consistent with Racket's built-in @racketlink[f:foldl "foldl"], and a value of @racket['bab] corresponding to the accumulator always being passed first, consistent with the version of @racketlink[d:foldl "foldl"] found in @racket[data/collection] and also in some other functional languages like Haskell.

 In many common cases, modulating the folding direction and/or the argument order does not make a difference to the result. Specifically, in those cases where the operation is @hyperlink["https://en.wikipedia.org/wiki/Commutative_property"]{commutative} and @hyperlink["https://en.wikipedia.org/wiki/Closure_(mathematics)"]{closed}, it doesn't matter whether you use @racket[foldl] or @racket[foldr], or whether you use argument order @racket['abb] or @racket['bab]. The result would be the same. However, in cases where the operation is not closed, argument order becomes significant. As a general guideline, choose between @racket[foldl] and @racket[foldr] in cases where the operation is not commutative (a relatively common case, such as string concatenation), and between the two argument orders in cases where the operation isn't closed (a less common case).

 @racket[foldl] is equivalent to calling @racket[fold] with @racket[#:direction 'left], and @racket[foldr] is equivalent to calling @racket[fold] with @racket[#:direction 'right]. @racket[fold/steps] is equivalent to calling @racket[fold] with @racket[#:with-steps #t].

@examples[
    #:eval eval-for-docs
    (fold + '(1 2 3 4))
    (fold * '(1 2 3 4))
    (fold .. '("hi" " " "there"))
    (foldr + '(1 2 3 4))
    (foldl + '(1 2 3 4))
    (foldr + '(1 2 3 4) #:order 'bab)
    (foldl + '(1 2 3 4) #:order 'bab)
    (foldr .. '("hi" " " "there"))
    (foldl .. '("hi" " " "there"))
    (foldr cons '(1 2 3) '() #:order 'abb)
    (foldl cons '(1 2 3) '() #:order 'abb)
    (foldr cons '(1 2 3) '() #:order 'bab)
    (foldl cons '(1 2 3) '() #:order 'bab)
  ]
}

@deftogether[(
  @defproc[(fold/steps [f (-> any/c any/c any/c)]
                       [vs (sequenceof any/c)]
                       [base any/c #f]
                       [#:order order (one-of/c 'abb 'bab) 'abb]
                       [#:direction direction (one-of/c 'left 'right) 'right])
           any/c]
  @defproc[(foldl/steps [f (-> any/c any/c any/c)]
                        [vs (sequenceof any/c)]
                        [base any/c #f]
                        [#:order order (one-of/c 'abb 'bab) 'abb])
           any/c]
  @defproc[(foldr/steps [f (-> any/c any/c any/c)]
                        [vs (sequenceof any/c)]
                        [base any/c #f]
                        [#:order order (one-of/c 'abb 'bab) 'abb])
           any/c]
  )]{

 Similar to @racketlink[d:foldl/steps "foldl/steps"], but, like @racket[fold], infers the relevant @racketlink[id "identity"] element and uses it as the base value, if none is provided. @racket[foldl/steps] is equivalent to calling @racket[fold/steps] with @racket[#:direction 'left], and @racket[foldr/steps] is equivalent to calling @racket[fold/steps] with @racket[#:direction 'right].

@examples[
    #:eval eval-for-docs
    (->list (fold/steps + '(1 2 3 4)))
    (->list (foldr/steps + '(1 2 3 4)))
    (->list (foldl/steps + '(1 2 3 4)))
    (->list (foldr/steps * '(1 2 3 4)))
    (->list (foldl/steps * '(1 2 3 4)))
    (->list (foldr/steps .. '("hi" " " "there")))
    (->list (foldl/steps .. '("hi" " " "there")))
  ]
}

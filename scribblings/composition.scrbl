#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[relation/composition
                    relation/type
                    relation/logic
                    racket/generic
					racket/undefined
                    (except-in racket +
                                      -
                                      *
                                      /
                                      foldl
                                      foldr
                                      length
                                      append
                                      map
                                      sequence?)
                    (only-in racket (foldl f:foldl)
                                    (foldr f:foldr)
                                    (append b:append))
                    (only-in data/collection length
                                             repeat
                                             sequenceof
                                             sequence?
                                             map
                                             gen:sequence
                                             (foldl d:foldl)
                                             (foldl/steps d:foldl/steps))]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps)
                                           (only-in racket/math sqr)
                                           relation
                                           racket/set
                                           racket/stream))))

@title{Composing Operations}

@defmodule[relation/composition]

@margin-note{This module was formerly named @racket[relation/algebraic]. Any code using @racket[relation/algebraic] directly should be changed to use @racket[relation/composition] instead. The former alias is still provided alongside the new one for backwards compatibility, but will be removed in a future version.}

Generic algebraic operators for composing data.

The built-in operators @racket[+] and @racket[*] operate on numbers specifically. Often, however, we are interested in performing operations "similar" to these for datatypes that aren't numbers, for which we would resort to type-specific operators like @racketlink[b:append "append"] for lists.

This module generalizes the standard algebraic operators to work on any type that supports a "canonical" notion of addition, multiplication, or concatenation. This allows our intuitions about addition and other forms of composition to extend over all appropriate types via the use of the common generic operators @racket[+], @racket[*] and @racket[..].

@section[#:tag "composition:interfaces"]{Interfaces}

This module provides three generic interfaces -- @racket[gen:appendable], @racket[gen:multipliable], and @racket[gen:addable]. These are meant to represent the canonical "idea" of the operations of concatenation, multiplication and addition, respectively, whose behavior may be customized for each type via these generic interfaces, and used via the common operators @racket[..] (concatenation), @racket[*] (multiplication), and @racket[+] (addition).

In order to support generic composition seamlessly, all of the composition interfaces support a generic (rather than type- and operation-specific) @hyperlink["https://en.wikipedia.org/wiki/Identity_element"]{identity} value that is employed in cases where type information is not available.

@defthing[ID composition-identity?]{
 The special value @racket[ID] serves as the generic identity value for all composition operations when the type of the operands is not known. It may appear at intermediate stages of a computation when there isn't sufficient information to infer a type-specific identity. Any value when composed with @racket[ID] yields itself.

@examples[
    #:eval eval-for-docs
    (+ 5 ID)
    (* ID 5)
    (+)
    (apply * '())
  ]
}

In the event no operands are received in the course of a computation, the result of composition would be @racket[ID], which would not be a usable result in utilities that are expecting a specific type such as a string. In such cases, the result could be converted to the expected type using one of the transformers in @seclink["type:transformers" #:doc '(lib "relation/scribblings/relation.scrbl")]{relation/type} such as @racket[->string]. If you are not using a built-in type but rather a @seclink["define-struct" #:doc '(lib "scribblings/guide/guide.scrbl")]{custom type}, however, you could use the following more general utility to "reify" the generic identity value to a type of your choosing:

@defproc[(reify [v any/c]
                [example any/c]
                [op procedure? ..])
         any/c]{

 "Reifies" a value to a specific type. If the value is already a tangible value (i.e. anything other than @racket[ID]), then it is returned without modification. Otherwise, the identity value for the desired type (indicated by supplying an arbitrary @racket[example] of this type) for the operation @racket[op] is returned. Custom types are expected to implement one of the canonical algebraic operations (e.g. @racket[gen:appendable]) in order to leverage this utility.

@examples[
    #:eval eval-for-docs
    (reify ID 3)
    (reify ID 3 *)
    (reify ID "cherry")
    (reify ID (list))
    (reify "hi" (list))
    (reify '(1 2 3) "")
  ]
}

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

 To implement this interface for custom types, the following methods need to be implemented, unless the type already implements another interface for which a default implementation exists for @racket[gen:appendable] (such as @racket[gen:sequence]) and if more specific handling is not needed for the custom type.

 @defproc[(append [a appendable?]
                  [b appendable?])
          appendable?]{

 A function taking two arguments that composes them in the natural "append-like" operation for the type. Both arguments are expected to be instances of the structure type to which the generic interface is associated (or a subtype of the structure type).

 In addition to providing a definition of concatenation appropriate to the type, implementations of this method must also handle the special value @racket[ID] in the following way: if the operand @racket[b] is @racket[eq?] to @racket[ID], then the result of the function must be @racket[a].
 }

 @defproc[(appendable-identity [a appendable?])
          appendable?]{

 A function that returns the identity value for the type, for the @racket[append] operation. The identity value is that value which, when composed with other values of the same type under the @racket[append] operation, yield those other values as if there had been no composition. The identity value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(appendable-inverse [a appendable?])
          appendable?]{

 A function that returns the inverse value for the type, for the @racket[append] operation. The inverse value is that value which, when composed with other values of the same type under the @racket[append] operation, yields the identity value. The inverse value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).

 Providing an implementation for this method is optional, and most types would usually not define an inverse for an append-like operation. That is, in mathematical terms, append-like operations typically form an algebraic @hyperlink["https://en.wikipedia.org/wiki/Semigroup"]{semigroup} or @hyperlink["https://en.wikipedia.org/wiki/Monoid"]{monoid} rather than a @hyperlink["https://en.wikipedia.org/wiki/Group_(mathematics)"]{group}.
 }

}

@subsection{Multiplication}

@defthing[gen:multipliable any/c]{

 A @tech/reference{generic interface} that represents any object for which a "multiplication-like" operation can be defined. The following built-in types have implementations for @racket[gen:multipliable]:

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

 In addition to providing a definition of multiplication appropriate to the type, implementations of this method must also handle the special value @racket[ID] in the following way: if the operand @racket[b] is @racket[eq?] to @racket[ID], then the result of the function must be @racket[a].
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

 In addition to providing a definition of addition appropriate to the type, implementations of this method must also handle the special value @racket[ID] in the following way: if the operand @racket[b] is @racket[eq?] to @racket[ID], then the result of the function must be @racket[a].
 }

 @defproc[(addable-identity [a addable?])
          addable?]{

 A function that returns the identity value for the type, for the @racket[add] operation. The identity value is that value which, when composed with other values of the same type under the @racket[add] operation, yield those other values as if there had been no composition. The identity value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).
 }

 @defproc[(addable-inverse [a addable?])
          addable?]{

 A function that returns the inverse value for the type, for the @racket[add] operation. The inverse value is that value which, when composed with other values of the same type under the @racket[add] operation, yields the identity value. The inverse value is expected to be an instance of the structure type to which the generic interface is associated (or a subtype of the structure type).

 Providing an implementation for this method is required; an addition operation must admit an inverse. That is, in mathematical terms, addition-like operations are expected to form an algebraic @hyperlink["https://en.wikipedia.org/wiki/Group_(mathematics)"]{group}.
 }

}

@section[#:tag "composition:utilities"]{Utilities}

@deftogether[(@defproc[(.. [v appendable?]
                           ...)
                       appendable?]
              @defproc[(∘ [v appendable?]
                          ...)
                       appendable?]
              @defproc[(..> [v appendable?]
                            ...)
                       appendable?]
 )]{

 Append the provided values together, using the canonical "append-like" operation on the data based on its type. @racket[..] and its alias @racket[∘] compose right-to-left, while @racket[..>] composes left-to-right. The special value @racket[ID] serves as the generic identity value for all composition operations when the type of the operands is not known. In particular, this value is the result when no operands are provided.

@examples[
    #:eval eval-for-docs
    (.. "hi" " " "there")
    (.. '(1 2 3) '(4 5 6))
    (.. (hash 'a 1 'b 2) (hash 'c 3))
    ((.. ->string +) 3 4)
    ((..> + ->string) 3 4)
    (∘ "hi" " " "there")
  ]
}

@defproc[(* [v multipliable?]
            ...)
         multipliable?]{

 Multiply the provided values together, using the canonical "multiplication-like" operation on the data based on its type. The special value @racket[ID] serves as the generic identity value for all composition operations when the type of the operands is not known. In particular, this value is the result when no operands are provided.

@examples[
    #:eval eval-for-docs
    (* 1 2 3 4)
  ]
}

@defproc[(+ [v addable?]
            ...)
         addable?]{

 Add the provided values together, using the canonical "addition-like" operation on the data based on its type. The special value @racket[ID] serves as the generic identity value for all composition operations when the type of the operands is not known. In particular, this value is the result when no operands are provided.

@examples[
    #:eval eval-for-docs
    (+ 1 2 3 4)
    (+ #(1 2 3) #(1 2 3) #(1 2 3))
  ]
}

@defproc[(id [operation procedure?])
         procedure?]{

 Produces the "identity" procedure for the given canonical operation, which, when evaluated for a particular value, yields the @hyperlink["https://en.wikipedia.org/wiki/Identity_element"]{identity} value for that type under the indicated operation.

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

 Produces the "inverse" procedure for the given canonical operation, which, when evaluated for a particular value, yields the @hyperlink["https://en.wikipedia.org/wiki/Inverse_element"]{inverse} value for that type under the indicated operation.

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

@deftogether[(
  @defproc[(fold [f procedure?]
                 [seqs (listof (sequenceof any/c))]
                 [#:into base any/c undefined]
                 [#:order order (one-of/c 'abb 'bab) 'abb]
                 [#:direction direction (one-of/c 'left 'right) 'right]
                 [#:with-steps? with-steps? boolean? #f])
           any/c]
  @defproc[(foldl [f procedure?]
                  [seqs (listof (sequenceof any/c))]
                  [#:into base any/c undefined]
                  [#:order order (one-of/c 'abb 'bab) 'abb]
                  [#:with-steps? with-steps? boolean? #f])
           any/c]
  @defproc[(foldr [f procedure?]
                  [seqs (listof (sequenceof any/c))]
                  [#:into base any/c undefined]
                  [#:order order (one-of/c 'abb 'bab) 'abb]
                  [#:with-steps? with-steps? boolean? #f])
           any/c]
  )]{

 Similar to @racketlink[f:foldl "foldl"] and @racketlink[f:foldr "foldr"], but infers the relevant @racketlink[id "identity"] element where possible and uses it as the @racket[base] value, if none is provided. The identity element is determined by considering the first element of the input sequence @racket[seqs] (or of the @emph{first} input sequence, if multiple sequences are provided) together with the given operation @racket[f].

 With folding operations there are two parameters that one may wish to tweak. The first is the direction of the fold, either left or right, for which one may use either @racket[foldl] or @racket[foldr]. The second is the order in which arguments are supplied to the folding function @racket[f], which may be controlled by the keyword argument @racket[#:order], with a value of @racket['abb] corresponding to the accumulator always being passed last, consistent with Racket's built-in @racketlink[f:foldl "foldl"], and a value of @racket['bab] corresponding to the accumulator always being passed first, consistent with the version of @racketlink[d:foldl "foldl"] found in @racket[data/collection] and also in some other functional languages like Haskell.

 In many common cases, modulating the folding direction and/or the argument order does not make a difference to the result. Specifically, in those cases where the operation is @hyperlink["https://en.wikipedia.org/wiki/Commutative_property"]{commutative} and @hyperlink["https://en.wikipedia.org/wiki/Closure_(mathematics)"]{closed}, it doesn't matter whether you use @racket[foldl] or @racket[foldr], or whether you use argument order @racket['abb] or @racket['bab]. The result would be the same. However, in cases where the operation is not closed, argument order becomes significant. As a general guideline, choose between @racket[foldl] and @racket[foldr] in cases where the operation is not commutative (a relatively common case, such as string concatenation), and between the two argument orders in cases where the operation isn't closed (a less common case, such as type constructors).

 @racket[foldl] is equivalent to calling @racket[fold] with @racket[#:direction 'left], and @racket[foldr] is equivalent to calling @racket[fold] with @racket[#:direction 'right]. @racket[fold/steps] is equivalent to calling @racket[fold] with @racket[#:with-steps? #t].

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
    (foldr cons '(1 2 3) #:into '() #:order 'abb)
    (foldl cons '(1 2 3) #:into '() #:order 'abb)
    (foldr cons '(1 2 3) #:into '() #:order 'bab)
    (foldl cons '(1 2 3) #:into '() #:order 'bab)
  ]
}

@deftogether[(
  @defproc[(fold/steps [f (-> any/c any/c any/c)]
                       [seqs (listof (sequenceof any/c))]
                       [#:into base any/c undefined]
                       [#:order order (one-of/c 'abb 'bab) 'abb]
                       [#:direction direction (one-of/c 'left 'right) 'right])
           any/c]
  @defproc[(foldl/steps [f (-> any/c any/c any/c)]
                        [seqs (listof (sequenceof any/c))]
                        [#:into base any/c undefined]
                        [#:order order (one-of/c 'abb 'bab) 'abb])
           any/c]
  @defproc[(foldr/steps [f (-> any/c any/c any/c)]
                        [seqs (listof (sequenceof any/c))]
                        [#:into base any/c undefined]
                        [#:order order (one-of/c 'abb 'bab) 'abb])
           any/c]
  )]{

 Similar to @racketlink[d:foldl/steps "foldl/steps"], but, like @racket[fold], infers the relevant @racketlink[id "identity"] element where possible and uses it as the @racket[base] value, if none is provided. @racket[foldl/steps] is equivalent to calling @racket[fold/steps] with @racket[#:direction 'left], and @racket[foldr/steps] is equivalent to calling @racket[fold/steps] with @racket[#:direction 'right].

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

@defproc[(onto [fs (sequenceof procedure?)]
               [v any/c]
               ...)
		 sequence?]{

  A kind of "dual" to the usual @racket[map] operation where we map @italic{values} under a @italic{function}, @racket[onto] instead maps @italic{functions} onto a @italic{value}. Specifically, this applies each function in the input sequence of functions to the provided arguments, independently, lazily yielding a corresponding sequence of results. Each of the input functions must have an arity that accepts the provided number of arguments.

@margin-note{This utility was formerly known as @racket[gather]. It is still provided under that name for backwards compatibility, but the old name will be removed in a future version. The new name was chosen to match a similar utility found in the @hyperlink["http://www.paulgraham.com/bel.html"]{Bel} dialect of Lisp.}

@examples[
    #:eval eval-for-docs
    (->list (onto (list add1 sub1 ->string) 0))
    (->list (onto (list + * min max) 7 6))
    (define (conjoin . fs)
      (.. all? (curry onto fs)))
    ((conjoin positive? even? integer?) 4)
    (define (n·xⁿ [n 0])
      (stream-cons (.. (curry * n)
                       (curryr expt n))
                   (n·xⁿ (add1 n))))
    (->list (take 10 (onto (n·xⁿ) 3)))
    (->list (take 10 (onto (map .. (repeat ->string) (n·xⁿ)) 3)))
  ]
}

@defproc[(join [vs (sequenceof appendable?)])
         appendable?]{

 Equivalent to @racket[(apply .. vs)], this stitches together a sequence containing elements of any @racketlink[gen:appendable]{appendable} type, for instance, @tech/reference{strings}, @tech/reference{lists}, or @seclink["procedures" "procedures" #:doc '(lib "scribblings/reference/reference.scrbl")].

@examples[
    #:eval eval-for-docs
    (join (list "hello" " " "there"))
    (join (stream '(1 2 3) '(4 5 6)))
    (join (list number->string add1 sqr))
    (join (list 1 2 3 4))
    (join (list #(1 2 3) #(1 2 3) #(1 2 3)))
    (join (list (stream 1 2 3) (stream 1 2 3) (stream 1 2 3)))
  ]
}

@defproc[(sum [vs (sequenceof addable?)])
         addable?]{

 Equivalent to @racket[(apply + vs)], this supports @tech/reference{numbers} in the usual way, but also supports any other @racketlink[gen:addable]{addable} type, for instance, @tech/reference{vectors}.

@examples[
    #:eval eval-for-docs
    (sum (list 1 2 3 4))
    (sum (list #(1 2 3) #(1 2 3) #(1 2 3)))
  ]
}

@defproc[(product [vs (sequenceof multipliable?)])
         multipliable?]{

 Equivalent to @racket[(apply * vs)], this supports @tech/reference{numbers} in the usual way, but also supports any other @racketlink[gen:multipliable]{multipliable} type.

@examples[
    #:eval eval-for-docs
    (product (list 1 2 3 4))
  ]
}

@deftogether[(
 @defproc[(power [v any/c]
                 [n integer?]
                 [op procedure? ..])
		  any/c]
 @defproc[(^ [v any/c]
             [n integer?])
          any/c]
  )]{

  Compose @racket[v] with itself @racket[n] times with the @racket[op] operation. If @racket[n] is negative, the result is the @racket[inverse] of the value computed with a positive exponent. This generalizes the idea of a numeric "power" to @hyperlink["https://en.wikipedia.org/wiki/Exponentiation#Monoids"]{any type} and composing operation. @racket[^] is a right-curried form of @racket[power], useful in cases where we want to abstract over the numeric power @racket[n] rather than the value @racket[v], for append-like compositions specifically (such as function composition).

@margin-note{Whenever a function produces an output of the same type as its input (i.e. in mathematical terms it is a self-map), it has a well-defined notion of "powers." For example, @racket[cdr] is a self-map on lists, but @racket[car] is not.}

@examples[
    #:eval eval-for-docs
    ((power add1 3) 5)
    (power "abc" 5)
    (power 2 3)
    (power 2 3 *)
    (power 2 -3 *)
    (power 2 -3 +)
    (((^ 3) add1) 4)
    (->list (((^ 3) rest) (list 1 2 3 4 5 6 7 8 9 10)))
  ]
}

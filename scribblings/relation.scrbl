#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         @for-label[racket]]

@title{Generic Relations}
@author{Siddhartha Kasivajhula}

@defmodule[relation]

This collection provides generic relations and type-agnostic operators. Out of the box, many Racket relations and operators are type-specific. For instance, @racket[<] operates specifically on numbers, conversion of any datatype to a string must use a type-specific transformer like @racket[symbol->string], and likewise @racket[+] operates specifically on numbers even though many datatypes sustain a natural notion of addition. This package provides a number of interfaces and utilities to override these default operators with generic versions. With a few exceptions, the generic operators provided in this collection are drop-in alternatives to the built-in ones.

@table-of-contents[]

@include-section["logic.scrbl"]
@include-section["equivalence.scrbl"]
@include-section["order.scrbl"]
@include-section["transform.scrbl"]
@include-section["algebraic.scrbl"]

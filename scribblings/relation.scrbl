#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         @for-label[racket]]

@title{Generic Relations}
@author{Siddhartha Kasivajhula}

@defmodule[relation]

This collection provides generic relations and type-agnostic operators. Many built-in Racket relations and operators are type-specific. For instance, @racket[<] operates specifically on numbers, conversion of any datatype to a string must use a type-specific transformer like @racket[symbol->string], and likewise @racket[+] operates specifically on numbers even though many datatypes sustain a natural notion of addition. This package provides a number of interfaces and utilities to override these default Racket operators with generic versions.

@table-of-contents[]

@include-section["comparable.scrbl"]
@include-section["transform.scrbl"]
@include-section["algebraic.scrbl"]

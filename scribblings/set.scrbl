#lang scribble/manual
@(require
 (for-label scapegoat-tree/set
            racket/base
            racket/contract
            data/order))

@title{Scapegoat Tree Sets}

@defmodule[scapegoat-tree/set]

A set type, implemented using the @racket[gen:set] interface on top of scapegoat tree dicts.

@section{Set Interface}

@defproc[(scapegoat-set? [obj any/c]) boolean?]{

Tests if an object is a scapegoat set or not.

}

@defproc[(make-scapegoat-set [order order? datum-order] [#:contract contract? (order-domain-contract order)]) scapegoat-set?]{

Returns a new empty set using the optional order object and contract.

}

@defproc[(scapegoat-set-contract [ss scapegoat-set?]) contract?]{

Returns the contract used on values stored in the set.

}

All other operations on scapegoat sets should use the generic set
interface functions for now.
#lang scribble/manual
@(require
 (for-label scapegoat-tree
            racket/base
            racket/contract
            data/order))

@title{Scapegoat Trees}

@defmodule[scapegoat-tree]

@section{Introduction}

This module provides a dictionary type implemented using
@hyperlink["https://en.wikipedia.org/wiki/Scapegoat_tree"]{scapegoat trees}.

A scapegoat tree implements the @code{gen:dict} and
@code{gen:ordered-dict} interfaces. Trees are immutable; side-effect
versions of functions update a pointer to the root of the tree as a
convienence.

@section{Dictionary Interface}

In addition to the @code{dict?} and @code{ordered-dict?} APIs,
scapegoat trees support the following functions.

@subsection{Parameters}

@defparam[scapegoat-tree-height-factor factor (and/c (>/c 0.5) (</c 1)) #:value 2/3]{

The height factor used in calculating if a tree needs to be
rebalanced. The higher the number, the more unbalanced the tree is
allowed to become.

}

@defparam[scapegoat-tree-rebalance-on-copy rebalance? boolean? #:value #t]{

If true, @code{dict-copy} and @code{scapegoat-tree-copy} return a
fully rebalanced tree instead of a straight copy of the original.

}

@subsection{Predicates}

@defproc[(scapegoat-tree? [obj any/c]) boolean?]{

Tests if an object is a scapegoat tree or not.

}

@defproc[(scapegoat-tree-iterator? [obj any/c]) boolean?]{

Tests if an object is a scapegoat tree iterator or not.

}

@subsection{Constructors}

@defproc[(make-scapegoat-tree [order order? datum-order]
                              [#:key-contract key-contract contract? (order-datum-contract order)]
                              [#:value-contract value-contract contract? any/c])
         scapegoat-tree?]{

Makes a new empty scapegoat tree. The tree uses @racket[order] to
order keys; in addition, the domain contract of @racket[order] is
combined with @racket[key-contract] to check keys.

}

@defproc[(scapegoat-tree-copy [s scapegoat-tree?])
         scapegoat-tree?]{

Returns a new copy of the given tree.

}

@defproc[(scapegoat-tree-clear [s scapegoat-tree?])
         scapegoat-tree?]{

Returns a new empty tree with the same order and contracts as the original.

}

@defform[(for/scapegoat-tree [#:order order order? order-datum]
                             [#:key-contract key-contract contract? (order-datum-contract order)]
                             [#:value-contract value-contract contract? any/c]
          (sequence-binding ...)
          body ...)] {

A @racket{for} comprehension that returns a scapegoat tree with the optionally
provided order and contracts. The body should return two values, used as the
key and value to be inserted into the returned scapegoat tree. Later keys
overrwrite earlier duplicates.

}

@defform[(for*/scapegoat-tree [#:order order order? order-datum]
                              [#:key-contract key-contract contract? (order-datum-contract order)]
                              [#:value-contract value-contract contract? any/c]
          (sequence-binding ...)
          body ...)] {

A @racket{for*} comprehension that returns a scapegoat tree with the optionally
provided order and contracts. The body should return two values, used as the
key and value to be inserted into the returned scapegoat tree. Later keys
overrwrite earlier duplicates.

}

@subsection{Queries}

@defproc[(scapegoat-tree-order [s scapegoat-tree?]) order?]{

Returns the @racket[order] object used to compare keys in the tree.

}

@defproc[(scapegoat-tree-key-contract [s scapegoat-tree?]) contract?]{

Returns the contract used on keys stored in the tree.

}

@defproc[(scapegoat-tree-value-contract [s scapegoat-tree?]) contract?]{

Returns the contract used on values stored in the tree.

}

@defproc[(scapegoat-tree-empty? [s scapegoat-tree?]) boolean?]{

Tests to see if the tree if empty or not.

}

@defproc[(scapegoat-tree-count [s scapegoat-tree?]) natural-number/c]{

Returns the number of elements stored in the tree.

}

@defproc[(scapegoat-tree-ref [s scapegoat-tree?] [key any/c] [default any/c (lambda () (error "key not found\n    key: " key))])
         any]{

Returns the value associated with the given tree. If not found and
@code{default} is a procedure, returns the value it returns,
otherwise returns @code{default}.

}

@subsubsection{Iterators}

@defproc[(scapegoat-tree-iterate-first [s scapegoat-tree?])
         (or/c scapegoat-tree-iterator? #f)]{

Returns an iterator to the first element of the tree. Elements are
returned in order through the iterator interface.

Returns @code{#f} if the tree is empty.

}

@defproc[(scapegoat-tree-iterate-next [s scapegoat-tree?] [i scapegoat-tree-iterator?])
         (or/c scapegoat-tree-iterator? #f)]{

Returns the iterator to the next element after the given position, or
@code{#f} if at the end of the tree.

}

@defproc[(scapegoat-tree-iterate-key [s scapegoat-tree?] [i scapegoat-tree-iterator?]) any]{

Returns the key of the indicated element of the tree.

}

@defproc[(scapegoat-tree-iterate-value [s scapegoat-tree?] [i scapegoat-tree-iterator?]) any]{

Returns the value of the indicated element of the tree.

}

@defproc[(scapegoat-tree-iterator->list [s scapegoat-tree?] [i scapegoat-tree-iterator?]) list?]{

Returns an alist of the elements of the tree starting with the given
iterator. The @code{car} of each element of the list is the key and the
@code{cdr} is the value.

}

@defproc[(scapegoat-tree-iterator->key-list [s scapegoat-tree?] [i scapegoat-tree-iterator?]) list?]{

Returns a list of the keys of the tree starting with the given iterator.

}

@defproc[(scapegoat-tree-iterator->value-list [s scapegoat-tree?] [i scapegoat-tree-iterator?]) list?]{

Returns a list of the values of the tree starting with the given iterator.

}

@subsection{Mutators}

@defproc[(scapegoat-tree-set [s scapegoat-tree?] [key any/c] [value any/c]) scapegoat-tree?]{

Returns a new tree with the given key and value inserted into it. If
the key already exists, the value is updated in the returned tree. The
original tree is unmodified.

}

@defproc[(scapegoat-tree-set! [s scapegoat-tree?] [key any/c] [value any/c]) any]{

Inserts the given key and value in the tree in-place. If the key already exists, the value is updated.

}


@defproc[(scapegoat-tree-delete [s scapegoat-tree?] [key any/c]) scapegoat-tree?]{

Returns a new tree with the given key removed from it. The original
tree is unmodified. It is not an error if the key was not present.

}

@defproc[(scapegoat-tree-delete! [s scapegoat-tree?] [key any/c]) any]{

Removes the given key from the tree in-place. It is not an error if the key was not present.

}

#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-syntax racket/base racket/path)
          (for-label scribble/base))

@title{Scapegoat Trees}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

This manual documents data structure modules available in the
@racketidfont{scapegoat-tree} collection.

@hyperlink["https://en.wikipedia.org/wiki/Scapegoat_tree"]{Scapegoat
trees} are a type of self-balancing binary trees that, unlike more
common ones like AVL trees and Red-Black trees that have to rebalance
themselves every time an element is inserted or deleted, only
rebalances when two sub-trees of an element get too far out of
balance. The rebalance factor for this implementation is a runtime
parameter, allowing for trees ranging from almost perfectly balanced
(Fast lookups, slower modifications), to ones that can look more like
linear linked lists (Faster modifications, slower lookups). One
strategy might be to have a high imbalance factor doing bulk
insertions, and then switch to a low one for regular use.

@local-table-of-contents[#:style 'immediate-only]

@;{--------}

@include-section["dict.scrbl"]
@include-section["set.scrbl"]

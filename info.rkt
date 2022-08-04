#lang info
(define collection "scapegoat-tree")
(define deps '("data-lib" "base"))
(define build-deps '("data-doc" "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/scapegoat-tree.scrbl" (multi-page) ("Scapegoat Trees"))))
(define pkg-desc "Dicts and Sets using Scapegoat Trees")
(define version "1.0")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))

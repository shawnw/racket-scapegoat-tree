#lang racket/base

(module+ test
  (require rackunit))

(require "main.rkt" data/order racket/contract racket/set racket/stream)

(provide (contract-out
          [scapegoat-set? predicate/c]
          [make-scapegoat-set (->* () (order? #:contract contract?) scapegoat-set?)]
          [scapegoat-set-contract (-> scapegoat-set? contract?)]))

(struct scapegoat-set ([st #:mutable])
  #:methods gen:set
  [(define (set-member? ss v)
     (if (scapegoat-tree-ref (scapegoat-set-st ss) v #f) #t #f))
   (define (set-add ss v)
     (scapegoat-set (scapegoat-tree-set (scapegoat-set-st ss) v #t)))
   (define (set-add! ss v)
     (scapegoat-tree-set! (scapegoat-set-st ss) v #t))
   (define (set-remove ss v)
     (scapegoat-set (scapegoat-tree-delete (scapegoat-set-st ss) v)))
   (define (set-remove! ss v)
     (scapegoat-tree-delete! (scapegoat-set-st ss) v))
   (define (set-empty? ss)
     (scapegoat-tree-empty? (scapegoat-set-st ss)))
   (define (set-count ss)
     (scapegoat-tree-count (scapegoat-set-st ss)))
   (define (set-first ss)
     (scapegoat-tree-iterate-key (scapegoat-tree-iterate-first (scapegoat-set-st ss))))
   (define (set->stream ss)
     (let ([st (scapegoat-set-st ss)])
       (stream-map (lambda (key val) key)
                   (scapegoat-tree-iterator->stream st (scapegoat-tree-iterate-first st)))))
   (define (set-copy ss)
     (scapegoat-set (scapegoat-tree-copy (scapegoat-set-st ss))))
   (define (set-copy-clear ss)
     (scapegoat-set (scapegoat-tree-clear (scapegoat-set-st ss))))
   (define (set-clear ss)
     (scapegoat-set (scapegoat-tree-clear (scapegoat-set-st ss))))
   (define (set-clear! ss)
     (set-scapegoat-set-st! (scapegoat-tree-clear (scapegoat-set-st ss))))
   (define (set->list ss)
     (let ([st (scapegoat-set-st ss)])
       (scapegoat-tree-iterator->key-list st (scapegoat-tree-iterate-first st))))])

(define (make-scapegoat-set [order datum-order] #:contract [contract (order-domain-contract order)])
  (scapegoat-set (make-scapegoat-tree order #:key-contract contract #:value-contract boolean?)))

(define (scapegoat-set-contract ss) (scapegoat-tree-key-contract (scapegoat-set-st ss)))

(module+ test
  (define empty (make-scapegoat-set))
  (check-pred scapegoat-set? empty)
  (check-pred generic-set? empty)
  (check-pred set-empty? empty)

  (define full (foldl (lambda (set elem) (set-add elem set))  empty '(1 10 2 9 3 8 4 7 5 6)))
  (check-false (set-empty? full))
  (check-true (set-member? full 5))
  (check-false (set-member? full 0))
  (check-equal? (set->list full) '(1 2 3 4 5 6 7 8 9 10)))

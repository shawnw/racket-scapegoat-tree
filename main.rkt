#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require data/order racket/contract racket/dict racket/generator racket/stream)

(provide (contract-out [scapegoat-tree? predicate/c]
                       [scapegoat-tree-iterator? predicate/c]
                       [scapegoat-tree-height-factor (parameter/c (and/c (>/c 1/2) (</c 1)))]
                       [scapegoat-tree-rebalance-on-copy (parameter/c boolean?)]
                       [make-scapegoat-tree (->* () (order? #:key-contract contract? #:value-contract contract?) scapegoat-tree?)]
                       [scapegoat-tree-order (-> scapegoat-tree? order?)]
                       [scapegoat-tree-key-contract (-> scapegoat-tree? contract?)]
                       [scapegoat-tree-value-contract (-> scapegoat-tree? contract?)]
                       [scapegoat-tree-empty? (-> scapegoat-tree? boolean?)]
                       [scapegoat-tree-count (-> scapegoat-tree? natural-number/c)]
                       [scapegoat-tree-ref (->* (scapegoat-tree? any/c) (any/c) any)]
                       [scapegoat-tree-set (-> scapegoat-tree? any/c any/c scapegoat-tree?)]
                       [scapegoat-tree-set! (-> scapegoat-tree? any/c any/c any)]
                       [scapegoat-tree-delete (-> scapegoat-tree? any/c scapegoat-tree?)]
                       [scapegoat-tree-delete! (-> scapegoat-tree? any/c any)]
                       [scapegoat-tree-copy (-> scapegoat-tree? scapegoat-tree?)]
                       [scapegoat-tree-clear (-> scapegoat-tree? scapegoat-tree?)]
                       [scapegoat-tree-iterate-first (-> scapegoat-tree? (or/c scapegoat-tree-iterator? #f))]
                       [scapegoat-tree-iterate-next (-> scapegoat-tree? scapegoat-tree-iterator? (or/c scapegoat-tree-iterator? #f))]
                       [scapegoat-tree-iterate-key (-> scapegoat-tree? scapegoat-tree-iterator? any)]
                       [scapegoat-tree-iterate-value (-> scapegoat-tree? scapegoat-tree-iterator? any)]
                       (scapegoat-tree-iterator->stream (-> scapegoat-tree? scapegoat-tree-iterator? stream?))
                       [scapegoat-tree-iterator->list (-> scapegoat-tree? scapegoat-tree-iterator? list?)]
                       [scapegoat-tree-iterator->key-list (-> scapegoat-tree? scapegoat-tree-iterator? list?)]
                       [scapegoat-tree-iterator->value-list (-> scapegoat-tree? scapegoat-tree-iterator? list?)]
                       ))

(define scapegoat-tree-height-factor
  (make-parameter 2/3
                  (lambda (val)
                    (if (and (real? val) (> val 1/2) (< val 1))
                        val
                        (raise-argument-error "scapegoat-tree-height-factor" "value out of range; must be real number between 0.5 and 1, given: " val)))))
(define scapegoat-tree-rebalance-on-copy (make-parameter #t))

(struct node (key val left right) #:transparent)
(struct scapegoat-tree ((count #:mutable) (max-count #:mutable) order key-contract value-contract (root #:mutable))
  #:methods gen:dict
  [(define (dict-ref st key [default (lambda () (error "key not found\n    key: " key))]) (scapegoat-tree-ref st key default))
   (define (dict-empty? st) (scapegoat-tree-empty? st))
   (define (dict-count st) (scapegoat-tree-count st))
   (define (dict-set st key v) (scapegoat-tree-set st key v))
   (define (dict-set! st key v) (scapegoat-tree-set! st key v))
   (define (dict-delete st key) (scapegoat-tree-delete st key))
   (define (dict-delete! st key) (scapegoat-tree-delete! st key))
   (define (dict-iterate-first st) (scapegoat-tree-iterate-first st))
   (define (dict-iterate-next st it) (scapegoat-tree-iterate-next st it))
   (define (dict-iterate-key st it) (scapegoat-tree-iterate-key st it))
   (define (dict-iterate-value st it) (scapegoat-tree-iterate-value st it))
   (define (dict-copy st) (scapegoat-tree-copy st))
   (define (dict-clear st) (scapegoat-tree-clear st))]

  #:methods gen:ordered-dict
  [(define (dict-iterate-least st) (scapegoat-tree-iterate-first st))
   (define (dict-iterate-greatest st) (scapegoat-tree-maximum-iterator st))
   (define (dict-iterate-least/>? st key) (scapegoat-tree-least-iterator st key '(>)))
   (define (dict-iterate-least/>=? st key) (scapegoat-tree-least-iterator st key '(> =)))
   (define (dict-iterate-greatest/<? st key) (scapegoat-tree-great-iterator st key '(<)))
   (define (dict-iterate-greatest/<=? st key) (scapegoat-tree-great-iterator st key '(< =)))])

(define (make-scapegoat-tree [order datum-order] #:key-contract [key-contract (order-domain-contract order)] #:value-contract [value-contract any/c])
  (scapegoat-tree 0 0 order (if (contract-equivalent? (order-domain-contract order) key-contract)
                                (order-domain-contract order)
                                (and/c (order-domain-contract order) key-contract))
                  value-contract #f))
(define (scapegoat-tree-empty? st) (= (scapegoat-tree-count st) 0))

(define (node-ref cmp node key default)
  (cond
    ((node? node)
     (case (cmp key (node-key node))
       ([=] (node-val node)) ; Match
       ([<] (node-ref cmp (node-left node) key default))
       ([>] (node-ref cmp (node-right node) key default))))
    ((procedure? default) (default))
    (else default)))

(define-syntax-rule (check-key-contract function st key)
  (unless ((scapegoat-tree-key-contract st) key)
    (raise-argument-error (symbol->string function) "key fails contract" key)))

(define-syntax-rule (check-order-contract function st key)
  (unless ((order-domain-contract (scapegoat-tree-order st)) key)
    (raise-argument-error (symbol->string function) "key fails ordering contract" key)))

(define-syntax-rule (check-value-contract function st v)
  (unless ((scapegoat-tree-value-contract st) v)
    (raise-argument-error (symbol->string function) "value fails contract" v)))

(define (scapegoat-tree-ref st key [default (lambda () (error "key not found\n    key: " key))])
  (check-order-contract 'scapegoat-tree-ref st key)
  (node-ref (scapegoat-tree-order st) (scapegoat-tree-root st) key default))

(define (size node)
  (if (node? node)
      (+ (size (node-left node)) (size (node-right node)) 1)
      0))

(define (rebalance cmp root size)
  (letrec ([arr (make-vector size)]
           [pack (lambda (node i)
                   (if (node? node)
                       (let ([i (pack (node-left node) i)])
                         (vector-set! arr i node)
                         (pack (node-right node) (add1 i)))
                       i))]
           [build (lambda (i ns)
                    (if (= ns 0)
                        #f
                        (let* ([m (quotient ns 2)]
                               [p (+ i m)])
                          (struct-copy node (vector-ref arr p) [left (build i m)] [right (build (add1 p) (- ns m 1))]))))])
    (pack root 0)
    (build 0 size)))

(define (scapegoat-tree-copy st)
  (if (scapegoat-tree-rebalance-on-copy)
      (struct-copy scapegoat-tree st [max-count (scapegoat-tree-count st)] [root (rebalance (scapegoat-tree-order st) (scapegoat-tree-root st) (scapegoat-tree-count st))])
      (struct-copy scapegoat-tree st)))

(define (scapegoat-tree-clear st)
  (struct-copy scapegoat-tree st [count 0] [max-count 0] [root #f]))

(define (scapegoat? left-size right-size)
  (let ([node-factor (* (scapegoat-tree-height-factor) (+ left-size right-size 1))])
    (or (> left-size node-factor)
        (> right-size node-factor))))

(define (node-insert q cmp root key v)
  (letrec ([helper
            (lambda (nod depth)
              (if (node? nod)
                  (case (cmp key (node-key nod))
                    ([=] (values (struct-copy node nod [val v]) depth 0 #f #t)) ; Existing key; overwrite.
                    ([<]
                     (let*-values ([(left-child child-depth left-size inserted? rebalanced?) (helper (node-left nod) (add1 depth))]
                                   [(right-size) (if rebalanced? 0 (size (node-right nod)))]
                                   [(size) (+ left-size right-size 1)])
                       (cond
                         (rebalanced? (values (struct-copy node nod [left left-child]) child-depth 0 inserted? rebalanced?))
                         ((scapegoat? left-size right-size) ; Scapegoat; rebalance
                          (values (rebalance cmp (struct-copy node nod [left left-child]) size) child-depth size inserted? #t))
                         (else (values (struct-copy node nod [left left-child]) child-depth size inserted? rebalanced?)))))
                    ([>]
                     (let*-values ([(right-child child-depth right-size inserted? rebalanced?) (helper (node-right nod) (add1 depth))]
                                   [(left-size) (if rebalanced? 0 (size (node-left nod)))]
                                   [(size) (+ left-size right-size 1)])
                       (cond
                         (rebalanced? (values (struct-copy node nod [right right-child]) child-depth 0 inserted? rebalanced?))
                         ((scapegoat? left-size right-size) ; Scapegoat; rebalance
                          (values (rebalance cmp (struct-copy node nod [right right-child]) size) child-depth size inserted? #t))
                         (else
                          (values (struct-copy node nod [right right-child]) child-depth size inserted? rebalanced?))))))
                  (values (node key v #f #f) depth 1 #t #f)))])
    (let-values ([(new-root depth size inserted? rebalanced?) (helper root 0)])
      (values new-root inserted?))))

(define (scapegoat-tree-set st key v)
  (check-key-contract 'scapegoat-tree-set st key)
  (check-value-contract 'scapegoat-tree-set st v)
  (let-values ([(new-root inserted?) (node-insert (scapegoat-tree-max-count st) (scapegoat-tree-order st) (scapegoat-tree-root st) key v)])
    (struct-copy scapegoat-tree st [root new-root] [count (+ (scapegoat-tree-count st) (if inserted? 1 0))] [max-count (+ (scapegoat-tree-max-count st) (if inserted? 1 0))])))

(define (scapegoat-tree-set! st key v)
  (check-key-contract 'scapegoat-tree-set! st key)
  (check-value-contract 'scapegoat-tree-set! st v)
  (let-values ([(new-root inserted?) (node-insert (scapegoat-tree-max-count st) (scapegoat-tree-order st) (scapegoat-tree-root st) key v)])
    (set-scapegoat-tree-root! st new-root)
    (when inserted?
      (set-scapegoat-tree-count! st (add1 (scapegoat-tree-count st)))
      (set-scapegoat-tree-max-count! st (add1 (scapegoat-tree-max-count st))))))

(define (need-rebalance-after-delete? st)
  (<= (sub1 (scapegoat-tree-count st)) (* (scapegoat-tree-height-factor) (scapegoat-tree-max-count st))))

(define (node-remove-min root)
  (if (node? (node-left root))
      (let-values ([(min-node new-left) (node-remove-min (node-left root))])
        (values min-node (struct-copy node root [left new-left])))
      (values (struct-copy node root [left #f] [right #f]) (node-right root))))

(define (node-remove cmp root key)
  (if (node? root)
      (case (cmp key (node-key root))
        ([=] ; the node to remove
         (cond
           ((and (eq? (node-left root) #f)
                 (eq? (node-right root) #f))
            (values #f #t)) ;; Leaf
           ((and (eq? (node-left root) #f)
                 (not (eq? (node-right root) #f)))
            (values (node-right root) #t)) ;; Only right child
           ((and (not (eq? (node-left root) #f))
                 (eq? (node-right root) #f))
            (values (node-left root) #t)) ;; Only left child
           (else ;; Two children
            (let-values ([(min-of-right new-right) (node-remove-min (node-right root))])
              (values (struct-copy node min-of-right [left (node-left root)] [right new-right]) #t)))))
        ([<]
         (let-values ([(new-left removed?) (node-remove cmp (node-left root) key)])
           (if removed?
               (values (struct-copy node root [left new-left]) #t)
               (values root #f))))
        ([>]
         (let-values ([(new-right removed?) (node-remove cmp (node-right root) key)])
           (if removed?
               (values (struct-copy node root [right new-right]) #t)
               (values root #f)))))
      (values root #f)))

(define (scapegoat-tree-delete st key)
  (check-order-contract 'scapegoat-tree-delete st key)
  (let-values ([(new-root removed?) (node-remove (scapegoat-tree-order st) (scapegoat-tree-root st) key)]
               [(new-n) (sub1 (scapegoat-tree-count st))])
    (cond
      ((and removed? (need-rebalance-after-delete? st))
       (struct-copy scapegoat-tree st
                    [root (rebalance (scapegoat-tree-order st) new-root new-n)]
                    [max-count new-n]
                    [count new-n]))
      (removed? (struct-copy scapegoat-tree st [root new-root] [count new-n]))
      (else st))))

(define (scapegoat-tree-delete! st key)
  (check-order-contract 'scapegoat-tree-delete st key)
  (let-values ([(new-root removed?) (node-remove (scapegoat-tree-order st) (scapegoat-tree-root st) key)]
               [(new-n) (sub1 (scapegoat-tree-count st))])
    (cond
      ((and removed? (need-rebalance-after-delete? st))
       (set-scapegoat-tree-root! st (rebalance (scapegoat-tree-order st) new-root new-n))
       (set-scapegoat-tree-max-count! st new-n)
       (set-scapegoat-tree-count! st new-n))
      (removed?
       (set-scapegoat-tree-root! st new-root)
       (set-scapegoat-tree-count! st new-n))
      (else (null)))))

(struct scapegoat-tree-iterator (st elems))

(define (generator->stream g)
  (let ([new-val (g)])
    (if (eq? (generator-state g) 'done)
      empty-stream
      (stream-cons new-val (generator->stream g)))))

(define (scapegoat-tree-iterate-first st)
  (if (scapegoat-tree-empty? st)
      #f
      (let ([g (generator ()
                          (let loop ([nod (scapegoat-tree-root st)])
                            (when (node? nod)
                              (loop (node-left nod))
                              (yield nod)
                              (loop (node-right nod)))))])
        (scapegoat-tree-iterator st (generator->stream g)))))

(define-syntax-rule (check-iterator function st st-i)
  (unless (eq? st (scapegoat-tree-iterator-st st-i))
    (raise-argument-error (symbol->string function) "Use of scapegoat tree iterator with different tree" st-i)))


(define (scapegoat-tree-iterate-next st st-i)
  (check-iterator 'scapegoat-tree-iterate-next st st-i)
  (if (stream-empty? (scapegoat-tree-iterator-elems st-i))
      #f
      (let ([new-elems (stream-rest (scapegoat-tree-iterator-elems st-i))])
        (if (stream-empty? new-elems)
            #f
            (struct-copy scapegoat-tree-iterator st-i [elems new-elems])))))

(define (scapegoat-tree-maximum-iterator st)
  (if (scapegoat-tree-empty? st)
      #f
      (let loop ([nod (scapegoat-tree-root st)])
        (if (node? (node-right nod))
            (loop (node-right nod))
            (scapegoat-tree-iterator st (stream nod))))))

(define (scapegoat-tree-least-iterator st key rel)
  (check-order-contract 'scapegoat-tree-least-iterator st key)
  (if (scapegoat-tree-empty? st)
      #f
      (let* ([cmp (scapegoat-tree-order st)]
             [g (generator ()
                           (let loop ([nod (scapegoat-tree-root st)])
                             (when (node? nod)
                               (when (memq (cmp (node-key nod) key) rel)
                                 (loop (node-left nod))
                                 (yield nod))
                               (loop (node-right nod)))))]
             [least (g)])
        (if (void? least)
            #f
            (scapegoat-tree-iterator st (stream-cons least (generator->stream g)))))))

(define (scapegoat-tree-great-iterator st key rel)
  (check-order-contract 'scapegoat-tree-great-iterator st key)
  (if (scapegoat-tree-empty? st)
      #f
      (let* ([cmp (scapegoat-tree-order st)]
             [g (generator ()
                           (let loop ([nod (scapegoat-tree-root st)])
                             (cond
                               ((eq? nod #f) (void))
                               ((memq (cmp (node-key nod) key) rel)
                                (cond
                                  ((and (node? (node-right nod))
                                        (memq (cmp (node-key (node-right nod)) key) rel))
                                   (loop (node-right nod)))
                                  (else
                                   (yield nod)
                                   (when (node? (node-right nod))
                                     (loop (node-right nod))))))
                               (else
                                (let loop2 ([nod nod])
                                  (when (node? nod)
                                    (when (node? (node-left nod))
                                      (loop (node-left nod)))
                                    (yield nod)
                                    (loop2 (node-right nod))))))))]
             [great (g)])
        (if (void? great)
            #f
            (scapegoat-tree-iterator st (stream-cons great (generator->stream g)))))))

(define (scapegoat-tree-iterate-key st st-i)
  (check-iterator 'scapegoat-tree-iterate-key st st-i)
  (node-key (stream-first (scapegoat-tree-iterator-elems st-i))))

(define (scapegoat-tree-iterate-value st st-i)
  (check-iterator 'scapegoat-tree-iterate-value st st-i)
  (node-val (stream-first (scapegoat-tree-iterator-elems st-i))))

(define (scapegoat-tree-iterator->stream st st-i)
  (check-iterator 'scapegoat-tree-iterator->stream st st-i)
  (stream-map (lambda (elem) (values (node-key elem) (node-val elem))) (scapegoat-tree-iterator-elems st-i)))

(define (scapegoat-tree-iterator->list st st-i)
  (check-iterator 'scapegoat-tree-iterator->list st st-i)
  (map (lambda (nod) (cons (node-key nod) (node-val nod))) (stream->list (scapegoat-tree-iterator-elems st-i))))

(define (scapegoat-tree-iterator->key-list st st-i)
  (check-iterator 'scapegoat-tree-iterator->key-list st st-i)
  (map node-key (stream->list (scapegoat-tree-iterator-elems st-i))))

(define (scapegoat-tree-iterator->value-list st st-i)
  (check-iterator 'scapegoat-tree-iterator->value-list st st-i)
  (map node-val (stream->list (scapegoat-tree-iterator-elems st-i))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define empty (make-scapegoat-tree))
  (check-pred scapegoat-tree? empty)
  (check-pred dict? empty)
  (check-pred ordered-dict? empty)
  (check-pred scapegoat-tree-empty? empty)
  (check-pred dict-empty? empty)
  (check-eqv? (scapegoat-tree-count empty) 0)
  (check-eqv? (dict-count empty) 0)

  (check-exn exn:fail? (lambda () (parameterize ([scapegoat-tree-height-factor 1.5]) #t)))
  (check-not-exn (lambda () (parameterize ([scapegoat-tree-height-factor 0.75]) #t)))

  (define real-tree (make-scapegoat-tree real-order #:value-contract symbol?))
  (check-exn exn:fail:contract? (lambda () (scapegoat-tree-set! real-tree 'foo 'bar)))
  (check-exn exn:fail:contract? (lambda () (scapegoat-tree-set! real-tree 1 "foo")))
  (check-not-exn (lambda () (scapegoat-tree-set! real-tree 1 'bar)))

  (define one (scapegoat-tree-set empty 1 'one))
  (check-false (scapegoat-tree-empty? one))
  (check-false (dict-empty? one))
  (check-eqv? (scapegoat-tree-count one) 1)
  (check-eqv? (dict-count one) 1)
  (check-eq? (scapegoat-tree-ref one 1 #f) 'one)
  (check-eq? (dict-ref one 1 #f) 'one)
  (check-false (scapegoat-tree-ref empty 1 #f))
  (check-false (dict-ref empty 1 #f))

  (define two (scapegoat-tree-set one 10 'ten))
  (define three (scapegoat-tree-set two 5 'five))

  (define two-it (dict-iterate-first two))
  (check-exn exn:fail:contract? (lambda () (dict-iterate-key three two-it)))
  (check-equal? (scapegoat-tree-iterator->key-list two two-it) '(1 10))

  (check-equal? (dict->list empty) '())
  (check-equal? (dict->list one) '((1 . one)))
  (check-equal? (dict->list two) '((1 . one) (10 . ten)))

  (define big-tree (foldl (lambda (p tree) (scapegoat-tree-set tree (car p) (cadr p))) (make-scapegoat-tree real-order)
                          '((5 five) (1 one) (6 six) (2 two) (7 seven) (3 three) (8 eight) (9 nine) (4 four))))
  (check-equal? (dict-count big-tree) 9)
  (check-equal? (dict-keys big-tree) '(1 2 3 4 5 6 7 8 9))
  (check-eq? (dict-ref big-tree 4 #f) 'four)
  (check-false (dict-ref big-tree 10 #f))

  ;; Make sure iteration doesn't invaliate existing iterator indexes
  (define big-it (scapegoat-tree-iterate-first big-tree))
  (check-true (and (= (scapegoat-tree-iterate-key big-tree big-it) 1)
                   (= (scapegoat-tree-iterate-key
                       big-tree
                       (scapegoat-tree-iterate-next big-tree big-it)) 2)))
  (define big-it-next (scapegoat-tree-iterate-next big-tree big-it))
  (check-eqv? (scapegoat-tree-iterate-key big-tree big-it) 1)
  (check-eqv? (scapegoat-tree-iterate-key big-tree big-it-next) 2)

  (check-equal? (dict-keys (scapegoat-tree-delete big-tree 8)) '(1 2 3 4 5 6 7 9))
  (check-equal? (dict-keys (scapegoat-tree-delete big-tree 3)) '(1 2 4 5 6 7 8 9))
  (check-equal? (dict-keys (scapegoat-tree-delete big-tree 5)) '(1 2 3 4 6 7 8 9))

  (check-equal? (dict-iterate-key big-tree (dict-iterate-least big-tree)) 1)
  (check-equal? (dict-iterate-key big-tree (dict-iterate-greatest big-tree)) 9)
  (define tree-li (dict-iterate-least/>? big-tree 4))
  (check-equal? (dict-iterate-key big-tree tree-li) 5)
  (check-equal? (scapegoat-tree-iterator->key-list big-tree tree-li) '(5 6 7 8 9))
  (define tree-li2 (dict-iterate-least/>=? big-tree 6))
  (check-equal? (dict-iterate-key big-tree tree-li2) 6)
  (check-equal? (scapegoat-tree-iterator->key-list big-tree tree-li2) '(6 7 8 9))
  (define tree-gi (dict-iterate-greatest/<? big-tree 4))
  (check-equal? (dict-iterate-key big-tree tree-gi) 3)
  (check-equal? (scapegoat-tree-iterator->key-list big-tree tree-gi) '(3 4 5 6 7 8 9))
  (check-equal? (dict-iterate-key big-tree (dict-iterate-greatest/<? big-tree 9)) 8)
  (define tree-gi2 (dict-iterate-greatest/<=? big-tree 5))
  (check-equal? (dict-iterate-key big-tree tree-gi2) 5)
  (check-equal? (scapegoat-tree-iterator->key-list big-tree tree-gi2) '(5 6 7 8 9))
)

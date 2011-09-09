#lang racket
(define N 15385)

; input-port -> (listOf pair)
(define (load-edges port)
  (for/list ([node-from (in-producer read eof port)]
             [node-to (in-producer read eof port)])
    (cons node-from node-to)))

; (listOf cons) -> (hash ((number . list) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hasheq)])
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-from node-to)
         (begin
           ; add node-to to node-from's adjacency list
           (hash-update! adjacencies node-from
                         (λ (node-list)
                           (cons node-to node-list)) '())
           ; add node-from to node-to's adjacency list
           (hash-update! adjacencies node-to
                         (λ (node-list)
                           (cons node-from node-list)) '()))]))
    adjacencies))
;-----------------------------------------------------------------------

(define adj/old
  (call-with-input-file "as_graph.txt"
    (λ (in)
      (read in)
      (edges->adjacencies (load-edges in)))))

(define adj/new (hash-copy adj/old))
(for ([(k v) (in-hash adj/new)])
  (hash-set! adj/new k (make-hasheq (for/list ([i (in-list v)])
                                      (cons i null)))))

(define adj/new-alt (hash-copy adj/new))
;-----------------------------------------------------------------------

(define (detach-nodes!/old adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-lst) (in-hash adjacencies)])
    (hash-set! adjacencies n (remq node adj-lst))))

(define (detach-nodes!/new adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-hsh) (in-hash adjacencies)])
    (hash-remove! adj-hsh node)))

(define (detach-nodes!/new-alt adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node)
    (for ([(n adj-hsh) (in-hash adjacencies)])
      (hash-remove! adj-hsh node))))

(define (random-nodes amount)
  (for/list ([_ (in-range amount)])
    ; random number in range [0; N-1]
    (random N)))
;-----------------------------------------------------------------------
(let ([detach-list/1 (random-nodes 3200)]
      [detach-list/2 (random-nodes 3200)])
  ;(time (detach-nodes!/old     adj/old     nodes-to-be-detached))
  (time (detach-nodes!/new     adj/new     detach-list/1))
  (time (detach-nodes!/new-alt adj/new-alt detach-list/1))
  (time (detach-nodes!/new     adj/new     detach-list/2))
  (time (detach-nodes!/new-alt adj/new-alt detach-list/2)))
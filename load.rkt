#lang racket/base
(require racket/match
         "graph.rkt")

(provide (all-defined-out))

; string -> graph
(define (load-graph-from-file name)
  (call-with-input-file name
    (λ (in) (load-graph in))))

; Load an *undirected* graph structure from file.
; The first line is the number of nodes in the graph.
; The next lines are pair of numbers representing one
; symmetric edge.
;
; input-port -> graph
(define (load-graph port)
  (graph (read port)
         (edges->adjacencies (load-edges port))))

; The adjacencies are kept in a hash of hashes for
; improved performance on node detachment.
; The first level hash keys are nodes, and the values
; are hashes of (number . null).
; For instance, an edge 1<->2 becomes:
;
;   '#hasheq((1 . #hasheq((2 . ())))
;            (2 . #hasheq((1 . ()))))
;
; (listOf pair) -> (hasheq ((number . hasheq) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hasheq)])
    ; add node `to' to `from''s adjacency list
    (define (add-adjacency! from to)
      (hash-update! adjacencies from
                    (λ (node-hash)
                      (hash-set! node-hash to null)
                      node-hash) (make-hasheq)))
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-1 node-2)
         (begin
           (add-adjacency! node-1 node-2)
           (add-adjacency! node-2 node-1))]))
    adjacencies))

; input-port -> (listOf pair)
(define (load-edges port)
  (for/list ([node-from (in-producer read eof port)]
             [node-to (in-producer read eof port)])
    (cons node-from node-to)))
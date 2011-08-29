#lang racket/base
(require racket/match
         racket/port
         tests/eli-tester)

; an undirected graph structure
(struct graph (nodes# adjacencies) #:transparent)

; Load a graph structure from file.
; The first line is the number of nodes in the graph.
; The next lines are pair of numbers representing one symmetric edge.
;
; input-port -> graph
(define (load-graph port)
  (graph (read port) (edges->adjacencies (load-edges port))))

; (listOf cons) -> (hash ((number . list) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hash)])
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-from node-to)
         (begin
           (hash-update! adjacencies node-from (λ (node-list) (cons node-to node-list)) '())
           (hash-update! adjacencies node-to (λ (node-list) (cons node-from node-list)) '()))]))
    adjacencies))

; input-port -> (listOf cons)
(define (load-edges port)
  (for/list ([node-from (in-producer read eof port)]
             [node-to (in-producer read eof port)])
    (cons node-from node-to)))

; graph -> number
(define (connected-components# graph)
  0)


(call-with-input-file "small_graph.txt"
  (λ (in) (load-graph in)))

(let* ([nodes# 5]
       [edges '((1 . 2)
                (1 . 3))]
       [adjacencies (make-hash '((1 . (3 2))
                                 (2 . (1))
                                 (3 . (1))))]
       [test-graph (graph nodes# adjacencies)])
  (test
   (edges->adjacencies edges) => adjacencies
   
   (call-with-input-bytes #"1 2\n1 3"
     (λ (in) (load-edges in)))
   =>
   edges
   
   (call-with-input-bytes #"5\n1 2\n1 3"
     (λ (in) (load-graph in)))
   =>
   test-graph
   
   (connected-components# test-graph) => 1))
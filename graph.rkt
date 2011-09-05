#lang racket/base
(require racket/match
         racket/port
         racket/set
         tests/eli-tester
         (planet jaymccarthy/dijkstra))

;------------------------------------------------------------
; Definitions
;------------------------------------------------------------

; a graph consists of:
; nodes#      = number
; adjacencies = (hash ((number . list) ...))
(struct graph (nodes# adjacencies) #:transparent)

; Load an *undirected* graph structure from file.
; The first line is the number of nodes in the graph.
; The next lines are pair of numbers representing one
; symmetric edge.
;
; input-port -> graph
(define (load-graph port)
  (graph (read port) (edges->adjacencies (load-edges port))))

; (listOf cons) -> (hash ((number . list) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hasheq)])
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-from node-to)
         (begin
           (hash-update! adjacencies node-from (λ (node-list) (cons node-to node-list)) '())
           (hash-update! adjacencies node-to (λ (node-list) (cons node-from node-list)) '()))]))
    adjacencies))

; input-port -> (listOf pair)
(define (load-edges port)
  (for/list ([node-from (in-producer read eof port)]
             [node-to (in-producer read eof port)])
    (cons node-from node-to)))


;------------------------------------------------------------
; Algorithms
;------------------------------------------------------------

(define (graph-shortest-path g src [stop? (lambda (node) #f)])
  (shortest-path (lambda (node) (hash-ref (graph-adjacencies g) node '())) ; node-edges
                 (lambda (edge) 1)                                         ; edge-weight
                 (lambda (edge) edge)                                      ; edge-next
                 src
                 stop?))

(define (connected-component g node)
  (define-values (dist _)
    (graph-shortest-path g node))
  (list->set (hash-keys dist)))

; graph -> number
(define (connected-components# g)
  (let-values ([(components _)
   (for/fold ([components 0]
              [visited (set)])
             ([node (in-range 1 (add1 (graph-nodes# g)))]
              #:when (not (set-member? visited node)))
     (values
      (add1 components)
      (set-union visited (connected-component g node))))])
    components))


;------------------------------------------------------------
; Computations
;------------------------------------------------------------

(define small-graph
  (call-with-input-file "small_graph.txt"
    (λ (in) (load-graph in))))

(define-values (dist1 prev1)
  (graph-shortest-path small-graph 10))

small-graph
dist1
prev1
(shortest-path-to prev1 5)
(connected-component small-graph 10)
(connected-component small-graph 1)


(define as-graph
  (call-with-input-file "as_graph.txt"
    (λ (in) (load-graph in))))

(connected-components# as-graph)


;------------------------------------------------------------
; Tests
;------------------------------------------------------------

(define (all-tests)
  (let* ([nodes# 5]
         [edges '((1 . 2)
                  (1 . 3))]
         [adjacencies (make-hasheq '((1 . (3 2))
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
     
     (connected-components# test-graph) => 3)))

(all-tests)
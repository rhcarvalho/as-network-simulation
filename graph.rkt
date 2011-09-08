#lang racket/base
(require racket/list
         racket/match
         racket/port
         racket/set
;         data/queue             ;; for BFS implementation
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
           ; add node-to to node-from's adjacency list
           (hash-update! adjacencies node-from
                         (λ (node-list)
                           (cons node-to node-list)) '())
           ; add node-from to node-to's adjacency list
           (hash-update! adjacencies node-to
                         (λ (node-list)
                           (cons node-from node-list)) '()))]))
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
  (list->seteq (hash-keys dist)))

; Very slow and inneficient implementation
;
; graph -> (listOf seteq)
(define (connected-components g)
  (define-values (components _)
    (for/fold ([components '()]
               [visited (seteq)])
      ([node (in-range 1 (add1 (graph-nodes# g)))]
       #:when (not (set-member? visited node)))
      (let ([component (connected-component g node)])
        (values
         (cons component components)
         (set-union visited component)))))
  components)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (connected-component/fast g node)
  'bfs)

; This was intended to be faster than `connected-components'
; but it's not really better.
; I must consider first implementing `connected-component/fast'
; using BFS, then check again how it performs...
;
; graph -> (listOf seteq)
(define (connected-components/2 g)
  (let ([nodes# (graph-nodes# g)])
    (let loop ([unvisited (list->seteq (build-list nodes# add1))]
               [components '()])
      (if (set-empty? unvisited)
          components
          (let* ([chosen (set-item unvisited)]
                 [component (connected-component g chosen)])
            (loop (set-subtract unvisited component)
                  (cons component components)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; (listOf seteq) number -> number
(define (%-of-nodes/largest-component components nodes#)
  (* 100.0
     (/ (set-count (argmax set-count components))
        nodes#)))

;------------------------------------------------------------

(define (detach-nodes! g nodes)
  (let ([adjacencies (graph-adjacencies g)])
    (for ([node (in-list nodes)])
      (hash-remove! adjacencies node))
    (for* ([node (in-list nodes)]
           [(n adj-lst) (in-hash adjacencies)])
      (hash-set! adjacencies n (remq node adj-lst)))))

(define (detach-random-nodes! g amount)
  (let ([nodes# (graph-nodes# g)])
    (detach-nodes! g (for/list ([_ (in-range amount)])
                       ; random number in range [1; nodes#]
                       (add1 (random nodes#))))))


;------------------------------------------------------------
; Helpers
;------------------------------------------------------------

; return one arbitrary item of the set
;
; set -> any/c
(define (set-item s)
  (for/first ([item (in-set s)]) item))

;------------------------------------------------------------
; Computations
;------------------------------------------------------------
#|
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
|#

(define as-graph
  (call-with-input-file "as_graph.txt"
    (λ (in) (load-graph in))))

(define as-graph2
  (call-with-input-file "as_graph.txt"
    (λ (in) (load-graph in))))

(define (process-graph g)
  (let* ([nodes# (graph-nodes# g)]
         [1%*nodes (truncate (/ nodes# 100))])
    (printf "The graph has ~a nodes.~n" nodes#)
    (printf "% nodes detached / # connected components / % nodes in the largest component~n")
    (printf "0\t\t   ~a\t\t\t    100~n" 1 #|(connected-components# as-graph)|#)
    (for ([i (in-range 10)])
      (detach-random-nodes! g 1%*nodes)
      (let ([components (connected-components g)])
        (printf "~a\t\t   ~a\t\t\t    ~a~n"
                (add1 i)
                (length components)
                (real->decimal-string
                 (%-of-nodes/largest-component components nodes#)))))))

(time
  (process-graph as-graph))

;; Compare these two after `connected-component/fast' is implemented
;(time (length (connected-components as-graph)))
;(time (length (connected-components/2 as-graph)))

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
     
     (length (connected-components test-graph)) => 3))
  (node-removal-tests))

(define (node-removal-tests)
  (let ([g-before (graph 3 (make-hasheq '((1 . (3 2))
                                          (2 . (1))
                                          (3 . (1)))))]
        [g-after (graph 3 (make-hasheq '((1 . (3))
                                         (3 . (1)))))])
    (detach-nodes! g-before '(2))
    (test
     (equal? g-before g-after))))

(all-tests)
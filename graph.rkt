#lang racket/base
(require racket/list
         racket/match
         racket/port
         racket/set
         data/queue
         tests/eli-tester)

;------------------------------------------------------------
; Definitions
;------------------------------------------------------------

; a graph consists of:
; nodes#      = number
; adjacencies = (hasheq ((number . hasheq) ...))
(struct graph (nodes# adjacencies) #:transparent)

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


;------------------------------------------------------------
; Algorithms
;------------------------------------------------------------

; Breadth-first search
;
; graph number -> seteq
(define (connected-component g source)
  ;---------------------------------------
  (define visited (seteq))
  (define (mark node)
    (set! visited (set-add visited node)))
  (define (marked? node)
    (set-member? visited node))
  ;---------------------------------------
  (define q (make-queue))
  (enqueue! q source)
  (mark source)
  (let ([adjacencies (graph-adjacencies g)])
    (do ()
      ((queue-empty? q))
      (let* ([node (dequeue! q)]
             [neighbors (hash-ref adjacencies node (make-hasheq))])
        (for ([neighbor (in-hash-keys neighbors)])
          (when (not (marked? neighbor))
            (mark neighbor)
            (enqueue! q neighbor))))))
  visited)

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
           [(n adj-hsh) (in-hash adjacencies)])
      (hash-remove! adj-hsh node))))

(define (random-nodes g amount)
  (let ([nodes# (graph-nodes# g)])
    (for/list ([_ (in-range amount)])
      ; random number in range [1; nodes#]
      (add1 (random nodes#)))))


;------------------------------------------------------------
; Computations
;------------------------------------------------------------

(define as-graph
  (call-with-input-file "as_graph.txt"
    (λ (in) (load-graph in))))

(define (process-graph g)
  (let* ([nodes# (graph-nodes# g)]
         [1%*nodes (truncate (/ nodes# 100))])
    (printf "The graph has ~a nodes.~n" nodes#)
    (printf "% nodes detached / # connected components / % nodes in the largest component~n")
    (printf "0\t\t   ~a\t\t\t    100.00~n" 1 #|(length (connected-components g))|#)
    (for ([i (in-range 10)])
      (detach-nodes! g (random-nodes g 1%*nodes))
      (let ([components (connected-components g)])
        (printf "~a\t\t   ~a\t\t\t    ~a~n"
                (add1 i)
                (length components)
                (real->decimal-string
                 (%-of-nodes/largest-component components nodes#)))))))

(time (process-graph as-graph))


;------------------------------------------------------------
; Tests
;------------------------------------------------------------

(let* ([nodes# 5]
       [edges '((1 . 2)
                (1 . 3))]
       [adjacencies (make-hasheq
                     `((1 . ,(make-hasheq `((3 . ,null)
                                            (2 . ,null))))
                       (2 . ,(make-hasheq `((1 . ,null))))
                       (3 . ,(make-hasheq `((1 . ,null))))))]
       [test-graph (graph nodes# adjacencies)])
  
  (define small-graph
    (call-with-input-file "small_graph.txt"
      (λ (in) (load-graph in))))
  
  (define (test-basic)
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
  
  (define (bfs-tests)
    (test
     (connected-component small-graph 6)
     =>
     (seteq 6 9 10)))
  
  (define (node-removal-tests)
    (let ([g-before (graph 3 (hash-copy adjacencies))]
          [g-after (graph 3 (make-hasheq
                             `((1 . ,(make-hasheq `((3 . ,null))))
                               (3 . ,(make-hasheq `((1 . ,null)))))))])
      (detach-nodes! g-before '(2))
      (test
       (equal? g-before g-after))))
  
  (define (all-tests)
    (test-basic)
    (node-removal-tests)
    (bfs-tests))
  
  (all-tests))
#lang racket/base
(require racket/list
         racket/match
         racket/port
         racket/set
         data/queue
         srfi/13
         tests/eli-tester)

;------------------------------------------------------------
; Definitions
;------------------------------------------------------------

; a graph consists of:
; nodes#      = number
; adjacencies = (hasheq ((number . hasheq) ...))
(struct graph (nodes# adjacencies) #:transparent)

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

; Remove nodes from the graph `g' destructively.
;
; graph (listOf number) -> void
(define (detach-nodes! g nodes)
  (let ([adjacencies (graph-adjacencies g)])
    (for ([node (in-list nodes)])
      (hash-remove! adjacencies node))
    (for* ([node (in-list nodes)]
           [(n adj-hsh) (in-hash adjacencies)])
      (hash-remove! adj-hsh node))))

; Return a list of `amount' random nodes.
;
; graph number -> list
(define (random-nodes g amount)
  (let ([nodes# (graph-nodes# g)])
    (for/list ([_ (in-range amount)])
      ; random number in range [1; nodes#]
      (add1 (random nodes#)))))

; Return a list of `pos' nodes sorted by highest degree.
;
; graph number [procedure] -> list
(define (most-connected-nodes g pos [proc car])
  (map proc (take
             (sort (hash->list (graph-adjacencies g))
                   >
                   #:key (compose hash-count cdr))
             pos)))


;------------------------------------------------------------
; Computations
;------------------------------------------------------------

; string -> void
(define (process-graph name)
  (let* ([g/random   (load-graph-from-file name)]
         [g/directed (load-graph-from-file name)]
         [nodes# (graph-nodes# g/random)]
         [1%*nodes (truncate (/ nodes# 100))])
    ;--------------------------------------------------------
    (define (print-table-line . items)
      (define (to-string item)
        (cond
          [(flonum? item) (real->decimal-string item)]
          [else (number->string item)]))
      (define (pad item len)
        (string-pad (to-string item) len))
      (apply printf "|~a |~a |~a |~a |~a |~n"
             (map pad items '(8 12 18 12 18))))
    ;--------------------------------------------------------
    (printf "The graph has ~a nodes.~n" nodes#)
    (printf ".-----------------------------------------------------------------------------.~n")
    (printf "|         |        random detachment        |        directed detachment      |~n")
    (printf "| % nodes | # connected | % nodes in the    | # connected | % nodes in the    |~n")
    (printf "|detached |  components | largest component |  components | largest component |~n")
    (printf "|-----------------------------------------------------------------------------|~n")
    (print-table-line 0 1 100.00 1 100.00)
    (for ([i (in-range 10)])
      (detach-nodes! g/random   (random-nodes         g/random   1%*nodes))
      (detach-nodes! g/directed (most-connected-nodes g/directed 1%*nodes))
      (let ([components/random   (connected-components g/random)]
            [components/directed (connected-components g/directed)])
        (print-table-line (add1 i)
                          (length components/random)
                          (%-of-nodes/largest-component components/random nodes#)
                          (length components/directed)
                          (%-of-nodes/largest-component components/directed nodes#))))
    (printf "·-----------------------------------------------------------------------------·~n")
    (newline)))

(time (process-graph "as_graph.txt"))


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
       [test-graph (graph nodes# adjacencies)]
       [small-graph (load-graph-from-file "small_graph.txt")])
  
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
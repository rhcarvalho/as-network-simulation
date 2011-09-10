#lang racket/base
(require racket/list
         racket/match
         racket/port
         racket/set
         data/queue             ;; for BFS implementation
         tests/eli-tester
         (planet jaymccarthy/dijkstra))

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
  (graph (read port) (edges->adjacencies (load-edges port))))

; (listOf cons) -> (hash ((number . list) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hasheq)])
    ; add node-to to node-from's adjacency list
    (define (add-adjacency from to)
      (hash-update! adjacencies from
                    (λ (node-hash)
                      (hash-set! node-hash to null)
                      node-hash) (make-hasheq)))
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-1 node-2)
         (begin
           (add-adjacency node-1 node-2)
           (add-adjacency node-2 node-1))]))
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
  (shortest-path (lambda (node) (hash-keys                        ; node-edges
                                 (hash-ref (graph-adjacencies g)
                                           node
                                           (make-hasheq))))
                 (lambda (edge) 1)                                ; edge-weight
                 (lambda (edge) edge)                             ; edge-next
                 src
                 stop?))

; graph number -> seteq
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

; Breadth-fist search
;
; graph number -> seteq
(define (connected-component/fast g source)
  (define visited (seteq))
  (define (mark node)
    (set! visited (set-add visited node)))
  (define (marked? node)
    (set-member? visited node))
  
  (define q (make-queue))
  (enqueue! q source)
  (mark source)
  (do ()
    ((queue-empty? q))
    (let ([v (dequeue! q)])
      (for ([w (in-hash-keys (hash-ref (graph-adjacencies g) v (make-hasheq)))])
        (when (not (marked? w))
          (mark w)
          (enqueue! q w)))))
  visited)

; graph -> (listOf seteq)
(define (connected-components/fast g)
  (define-values (components _)
    (for/fold ([components '()]
               [visited (seteq)])
      ([node (in-range 1 (add1 (graph-nodes# g)))]
       #:when (not (set-member? visited node)))
      (let ([component (connected-component/fast g node)])
        (values
         (cons component components)
         (set-union visited component)))))
  components)

; This was intended to be faster than `connected-components'
; but it's not really better.
; I must consider first implementing `connected-component/fast'
; using BFS, then check again how it performs...
;
; graph -> (listOf seteq)
(define (connected-components/choose connected-component g)
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
           [(n adj-hsh) (in-hash adjacencies)])
      (hash-remove! adj-hsh node))))

(define (random-nodes g amount)
  (let ([nodes# (graph-nodes# g)])
    (for/list ([_ (in-range amount)])
      ; random number in range [1; nodes#]
      (add1 (random nodes#)))))


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

(define (process-graph g)
  (let* ([nodes# (graph-nodes# g)]
         [1%*nodes (truncate (/ nodes# 100))])
    (printf "The graph has ~a nodes.~n" nodes#)
    (printf "% nodes detached / # connected components / % nodes in the largest component~n")
    (printf "0\t\t   ~a\t\t\t    100~n" 1 #|(connected-components# as-graph)|#)
    (for ([i (in-range 10)])
      (detach-nodes! g (random-nodes g 1%*nodes))
      (let ([components (connected-components g)])
        (printf "~a\t\t   ~a\t\t\t    ~a~n"
                (add1 i)
                (length components)
                (real->decimal-string
                 (%-of-nodes/largest-component components nodes#)))))))

`(time
  (process-graph as-graph))


`(time (detach-nodes! as-graph (random-nodes as-graph  1000)))

;; Compare these two after `connected-component/fast' is implemented
(time (length (connected-components      as-graph)))
(time (length (connected-components/fast as-graph)))

(time (length (connected-components/choose connected-component      as-graph)))
(time (length (connected-components/choose connected-component/fast as-graph)))

;------------------------------------------------------------
; Tests
;------------------------------------------------------------

(define (all-tests)
  (let* ([nodes# 5]
         [edges '((1 . 2)
                  (1 . 3))]
         [adjacencies (make-hasheq
                       `((1 . ,(make-hasheq `((3 . ,null)
                                              (2 . ,null))))
                         (2 . ,(make-hasheq `((1 . ,null))))
                         (3 . ,(make-hasheq `((1 . ,null))))))]
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
  (node-removal-tests)
  (bfs-tests))

(define (node-removal-tests)
  (let ([g-before (graph 3 (make-hasheq
                            `((1 . ,(make-hasheq `((3 . ,null)
                                                   (2 . ,null))))
                              (2 . ,(make-hasheq `((1 . ,null))))
                              (3 . ,(make-hasheq `((1 . ,null)))))))]
        [g-after (graph 3 (make-hasheq
                           `((1 . ,(make-hasheq `((3 . ,null))))
                             (3 . ,(make-hasheq `((1 . ,null)))))))])
    (detach-nodes! g-before '(2))
    (test
     (equal? g-before g-after))))

(define (bfs-tests)
  (define small-graph
    (call-with-input-file "small_graph.txt"
      (λ (in) (load-graph in))))
  (test
   (connected-component/fast small-graph 6)
   =>
   (seteq 6 9 10)))

(all-tests)
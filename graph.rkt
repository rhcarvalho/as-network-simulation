#lang racket/base
(require data/queue
         racket/generator
         racket/list
         racket/match
         racket/set
         srfi/13)

(provide
 print-graph-detachment-csv
 print-graph-detachment-table
 graph-stats)

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

;------------------------------------------------------------

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

; graph -> number
(define (graph-edges# g)
  (quotient (foldl + 0 (map hash-count (hash-values (graph-adjacencies g))))
            2))

; graph -> real
(define (average-degree g)
  (define adjacencies (graph-adjacencies g))
  (define (degree node)
    (hash-count (hash-ref adjacencies node (make-hasheq))))
  (exact->inexact
   (/ (foldl + 0 (map degree (hash-keys adjacencies)))
      (graph-nodes# g))))


;------------------------------------------------------------
; Computations
;------------------------------------------------------------

; string -> number generator
(define (process-graph name)
  (let* ([g/random   (load-graph-from-file name)]
         [g/directed (load-graph-from-file name)]
         [nodes# (graph-nodes# g/random)]
         [1%*nodes (truncate (/ nodes# 100))]
         [detacher/random (graph-detacher g/random
                                          random-nodes
                                          1%*nodes)]
         [detacher/directed (graph-detacher g/directed
                                            most-connected-nodes
                                            1%*nodes)]
         [next-line (generator ()
                      (for ([i (in-naturals 1)])
                        (yield (append (detacher/random)
                                       (cdr (detacher/directed))))))])
    (values nodes# next-line)))

; graph procedure number -> generator
(define (graph-detacher g take-nodes-proc amount)
  (generator ()
    (let ([nodes# (graph-nodes# g)])
      (for ([i (in-naturals 1)])
        (detach-nodes! g (take-nodes-proc g amount))
        (let ([components (connected-components g)])
          (yield (list i
                       (length components)
                       (%-of-nodes/largest-component components nodes#))))))))

; string -> list
(define (graph-stats name)
  (let* ([g (load-graph-from-file name)]
         [nodes# (graph-nodes# g)]
         [nodes# (graph-edges# g)]
         [cc# (length (connected-components g))]
         [avg-degree (average-degree g)])
    `(("# nodes" ,nodes#)
      ("# edges" ,nodes#)
      ("# connected components" ,cc#)
      ("average degree" ,avg-degree))))

; string -> void
(define (print-graph-detachment-table name)
  ;--------------------------------------------------------
  (define (print-table-line items)
    (define (to-string item)
      (cond
        [(flonum? item) (real->decimal-string item)]
        [else (number->string item)]))
    (define (pad item len)
      (string-pad (to-string item) len))
    (apply printf "|~a |~a |~a |~a |~a |~n"
           (map pad items '(8 12 18 12 18))))
  ;--------------------------------------------------------
  (let-values ([(nodes# next-line) (process-graph name)])
    (printf "The graph has ~a nodes.~n" nodes#)
    (printf ".-----------------------------------------------------------------------------.~n")
    (printf "|         |        random detachment        |        directed detachment      |~n")
    (printf "| % nodes | # connected | % nodes in the    | # connected | % nodes in the    |~n")
    (printf "|detached |  components | largest component |  components | largest component |~n")
    (printf "|-----------------------------------------------------------------------------|~n")
    (print-table-line '(0 1 100.00 1 100.00))
    (for ([i (in-range 10)])
      (print-table-line (next-line)))
    (printf "·-----------------------------------------------------------------------------·~n")
    (newline)))

; string -> void
(define (print-graph-detachment-csv name)
  (define (print-line items)
    (map display (add-between items ","))
    (newline))
  (let-values ([(nodes# next-line) (process-graph name)])
    (print-line (list "% nodes detached"
                      "# connected components (random)"
                      "% nodes in the largest component (random)"
                      "# connected components (directed)"
                      "% nodes in the largest component (directed)"))
    (print-line '(0 1 100.00 1 100.00))
    (for ([i (in-range 10)])
      (print-line (next-line)))))
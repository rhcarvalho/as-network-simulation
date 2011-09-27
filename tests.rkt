#lang racket/base
(require racket/port
         racket/set
         rackunit
         rackunit/gui)

(require/expose "graph.rkt"
                (graph
                 load-graph
                 load-graph-from-file
                 load-edges
                 edges->adjacencies
                 connected-component
                 connected-components
                 detach-nodes!))

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
  (test/gui
   (test-suite
    "Internet (AS) network detachment tests"
    
    (test-suite
     "Basic tests"
     (test-equal?
      "Can load edges"
      (call-with-input-bytes #"1 2\n1 3"
        (λ (in) (load-edges in))) edges)
     (test-equal?
      "Can create adjacencies list from edges"
      (edges->adjacencies edges) adjacencies)
     (test-equal?
      "Can load a complete graph"
      (call-with-input-bytes #"5\n1 2\n1 3"
        (λ (in) (load-graph in))) test-graph))
    
    (test-suite
     "BFS tests"
     (test-equal?
      "Can find a connected component"
      (connected-component small-graph 6) (seteq 6 9 10))
     (test-equal?
      "Number of connected components is correct"
      (length (connected-components test-graph)) 3))
    
    (test-suite
     "Node removal tests"
     (let ([g-before (graph 3 (hash-copy adjacencies))]  ; not correct, should be a deep copy
           [g-after (graph 3 (make-hasheq
                              `((1 . ,(make-hasheq `((3 . ,null))))
                                (3 . ,(make-hasheq `((1 . ,null)))))))])
       (test-case
        "Nodes are detached correctly"
        (detach-nodes! g-before '(2))
        (check-equal? g-before g-after)))))))
#lang racket/base
(require racket/port
         racket/set
         tests/eli-tester
         "graph.rkt")

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
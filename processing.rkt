#lang racket/base
(require racket/generator
         "graph.rkt"
         "load.rkt")

(provide (all-defined-out))

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
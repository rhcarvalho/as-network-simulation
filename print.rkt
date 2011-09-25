#lang racket/base
(require racket/list
         srfi/13
         "graph.rkt"
         "processing.rkt")

(provide (all-defined-out))

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
    (for ([i (in-range 2)])
      (print-line (next-line)))))
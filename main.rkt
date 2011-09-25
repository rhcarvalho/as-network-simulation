#lang racket/base
(require "print.rkt")

;(time (print-graph-detachment-table "as_graph.txt"))
(time (print-graph-detachment-csv "as_graph.txt"))
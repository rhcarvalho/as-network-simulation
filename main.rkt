#lang racket/base
(require "graph.rkt")

(time (print-graph-detachment-csv "as_graph.txt"))
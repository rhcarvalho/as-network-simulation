#! /usr/bin/env racket
#lang racket/base

(require "graph.rkt")

(graph-stats "as_graph.txt")
(time (print-graph-detachment-csv "as_graph.txt"))
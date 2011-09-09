#lang racket
(define N 15385)

(define-syntax-rule (report-memory expr ...)
  (let ([cust (make-custodian)])
    (parameterize ([current-custodian cust])
      (define r (begin expr ...))
      (collect-garbage)
      (displayln (current-memory-use cust))
      r)))


; input-port -> (listOf pair)
(define (load-edges port)
  (for/list ([node-from (in-producer read eof port)]
             [node-to (in-producer read eof port)])
    (cons node-from node-to)))

; (listOf cons) -> (hash ((number . list) ...))
(define (edges->adjacencies edges)
  (let ([adjacencies (make-hasheq)])
    (for ([edge (in-list edges)])
      (match edge
        [(cons node-from node-to)
         (begin
           ; add node-to to node-from's adjacency list
           (hash-update! adjacencies node-from
                         (λ (node-list)
                           (cons node-to node-list)) '())
           ; add node-from to node-to's adjacency list
           (hash-update! adjacencies node-to
                         (λ (node-list)
                           (cons node-from node-list)) '()))]))
    adjacencies))
;-----------------------------------------------------------------------

(define adj/old
  (report-memory
   (call-with-input-file "as_graph.txt"
     (λ (in)
       (read in)
       (edges->adjacencies (load-edges in))))))

(define adj/new (hash-copy adj/old))
(report-memory
 (for ([(k v) (in-hash adj/new)])
   (hash-set! adj/new k (make-hasheq (for/list ([i (in-list v)])
                                       (cons i #t))))))

(define adj/new2 (hash-copy adj/new))

(define adj/new3 (hash-copy adj/new))
(for ([(k v) (in-hash adj/new3)])
  (hash-set! adj/new3 k (list->seteq (hash-keys v))))
;-----------------------------------------------------------------------

(define (detach-nodes!/old adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-lst) (in-hash adjacencies)])
    (hash-set! adjacencies n (remq node adj-lst))))

(define (detach-nodes!/new adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-hsh) (in-hash adjacencies)])
    (hash-remove! adj-hsh node)))

(define (detach-nodes!/new2 adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-hsh) (in-hash adjacencies)])
    (hash-set! adj-hsh node #f)))

(define (detach-nodes!/new3 adjacencies nodes)
  (for ([node (in-list nodes)])
    (hash-remove! adjacencies node))
  (for* ([node (in-list nodes)]
         [(n adj-set) (in-hash adjacencies)])
    (hash-set! adjacencies n (set-remove adj-set node))))

(define (detach-random-nodes! detach-nodes! adjacencies amount)
  (detach-nodes! adjacencies (for/list ([_ (in-range amount)])
                               ; random number in range [0; N-1]
                               (random N))))
;-----------------------------------------------------------------------

(time (detach-random-nodes! detach-nodes!/old  adj/old  100))
(time (detach-random-nodes! detach-nodes!/new  adj/new  100))
(time (detach-random-nodes! detach-nodes!/new2 adj/new2 100))
(time (detach-random-nodes! detach-nodes!/new3 adj/new3 100))


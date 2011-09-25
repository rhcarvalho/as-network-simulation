#lang racket/base
(require racket/list
         racket/set
         data/queue)

(provide (all-defined-out)
         (struct-out graph))

;------------------------------------------------------------
; Definitions
;------------------------------------------------------------

; a graph consists of:
; nodes#      = number
; adjacencies = (hasheq ((number . hasheq) ...))
(struct graph (nodes# adjacencies) #:transparent)


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
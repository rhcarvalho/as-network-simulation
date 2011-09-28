#lang at-exp slideshow
(require racket/draw)

; ----- Image definitions -----
(define as-ncc-10 (bitmap "images/as-ncc-10.png"))
(define as-ncc-40 (bitmap "images/as-ncc-40.png"))
(define as-plcc-10 (bitmap "images/as-plcc-10.png"))
(define as-plcc-40 (bitmap "images/as-plcc-40.png"))


; ----- Config -----
(set-page-numbers-visible! #f)
(define charts-w (* 9/10 client-w))


; ----- Helper functions -----
(define (with-height h pict)
  (scale pict (/ h (pict-height pict))))

(define (with-width w pict)
  (scale pict (/ w (pict-width pict))))

(define (nice-title txt)
  (let* ([-title (bt txt)]
         [title (colorize -title (make-object color% 255 127 0))]
         [shadow (colorize -title "black")])
    (scale/improve-new-text
     (pin-under title
                2 2
                shadow)
     1.8)))


; ----- Slides -----
(slide
 #:name "Complex network attack analysis"
 (rb-superimpose
  (lb-superimpose
   (ct-superimpose
    full-page
    (vc-append gap-size
               (blank (/ client-h 4))
               (nice-title "Complex network")
               (nice-title "attack analysis")
               (hline (* 3/4 client-w) gap-size)
               (blank)
               @para[#:align 'center]{Study case: Internet (AS) graph}))
   (t "Rodolfo Carvalho"))
  (scale/improve-new-text
   (t "UFRJ - Redes Complexas - 28/09/2011")
   0.8)))
(slide
 #:title "AS graph specs"
 (item "Nodes: 32385")
 (item "Edges: 46823 (46736 w/o dups)")
 (item "Undirected")
 (item "Connected components: 1")
 (item "Average degree: 2.89"))

(slide
 #:title "Random attack simulation"
 (item "Repeat" (it "n") "times:")
 (subitem "``detach'' 1% of" (bt "randomly chosen") "nodes")
 (subitem "compute connected components")
 (subitem "compute % of nodes in the largest component")
 (blank (* 3 gap-size))
 'next
 @para{Detaching nodes means that edges are removed while the
       @bt{number of nodes} in the graph is kept@bt{constant}.})

(slide
 #:title "Directed attack simulation"
 (item "Repeat" (it "n") "times:")
 (subitem "``detach'' 1% of" (bt "specially chosen") "nodes")
 (subitem "compute connected components")
 (subitem "compute % of nodes in the largest component")
 (blank (* 3 gap-size))
 'next
 @para{The nodes with@bt{highest degrees} are chosen.
       Degrees are recomputed@bt{once} per iteration.})

(slide
 #:name "Results"
 (bt "Results"))

(slide (with-width charts-w as-ncc-10))
(slide (with-width charts-w as-ncc-40))
(slide (with-width charts-w as-plcc-10))
(slide (with-width charts-w as-plcc-40))

(slide
 #:title "Results"
 @item{Attacking high-degree nodes causes@bt{high damage}.
       It requires a relatively small number of attacks
       to@bt{split the Internet} into islands.}
 (blank)
 @item{The network is to some extent@bt{resistent} against
       random attacks. Attacking 40% of the graph randomly is
       way@bt{less effective} than a@bt{directed attack} on 1% of it.})

(slide
 #:title "Further reading"
 @item{Attack vulnerability of complex networks.
       @para{Holme, P., Kim, B. J., Yoon, C. N., and Han, S. K.
       Physical Review E, 2002 - APS}}
 @subitem[#:bullet (blank)]{Available at:
          @hyperlinkize{@t{http://arxiv.org/pdf/cond-mat/0202410}}})
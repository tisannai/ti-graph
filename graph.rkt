#lang racket

;; Module for Graph (DAG) based operations.

(provide graph%

         node-name
         node-olink
         node-data
         node-new
         node-new-with-data
         node-visit
         node-unvisit
         node-visited?
         edge-node
         edge-data

         dfs-common
         dfs
         dfs-with-env

         bfs-common
         bfs
         bfs-with-env

         topological-sort
         shortest-path-between-nodes
         longest-path-between-nodes
         )

;; ------------------------------------------------------------
;; Nodes and edges:

;; Edge definition.
(struct edge
  (
   node       ; Associated node
   data       ; Edge value
   )
  #:mutable)

;; Node definition.
;;
;; Data is collection of user data.
(struct node
  (
   name       ; Node name
   visited    ; Node visited (support for algos)
   ilink      ; Links left:  ( (<node> value) ... )
   olink      ; Links right: ( (<node> value) ... )
   data       ; Generic data storage.
   )
  #:mutable)

;; Create new node with name.
(define (node-new name)
  (node name #f empty empty #f))

;; Create new node with name and data.
(define (node-new-with-data name data)
  (node name #f empty empty data))

;; Visit node.
(define (node-visit node)
  (set-node-visited! node #t))

;; Unvisit node.
(define (node-unvisit node)
  (set-node-visited! node #f))

;; Return visit status.
(define (node-visited? node)
  (node-visited node))

;; Unvisit all nodes.
(define (node-unvisit-all nodes)
  (for ((node nodes))
    (node-unvisit node)))


;; ------------------------------------------------------------
;; Graph:

(define graph%
  (class object%
    (super-new)

    (init-field
     name:
     (node-list: empty)
     (node-hash: (make-hash))
     )

    (define/public (add id . rest)
      (for ((id (append (list id) rest)))
        (let ((node (node-new id)))
          (set! node-list: (append node-list: (list node)))
          (hash-set! node-hash: id node))))

    (define/public (add-with-data id data . rest)
      (let each ((spec (append (list id data) rest)))
        (when (pair? spec)
          (let ((node (node-new-with-data (car spec) (cadr spec))))
            (set! node-list: (append node-list: (list node)))
            (hash-set! node-hash: (car spec) node))
          (each (cddr spec)))))
    
    (define/public (connect a b . rest)
      (let each ((pair (append (list a b) rest)))
        (when (pair? pair)
          (let ((a (ref (car pair)))
                (b (ref (cadr pair))))
            (set-node-olink! a (append (node-olink a) (list (edge b 0))))
            (set-node-ilink! b (append (node-ilink b) (list (edge a 0)))))
          (each (cddr pair)))))
    
    (define/public (connect-with-data a b data . rest)
      (let each ((pair (append (list a b data) rest)))
        (when (pair? pair)
          (let ((a (ref (car pair)))
                (b (ref (cadr pair)))
                (data (caddr pair)))
            (set-node-olink! a (append (node-olink a) (list (edge b data))))
            (set-node-ilink! b (append (node-ilink b) (list (edge a data)))))
          (each (cdddr pair)))))
    
    (define/public (first-node)
      (car node-list:))

    (define/public (ref id)
      (hash-ref node-hash: id))

    (define/public (each-node fn)
      (for ((node node-list:))
        (apply fn (list node))))

    (define/public (unvisit-all-nodes)
      (each-node
       (lambda (node)
         (node-unvisit node))))

    ))


;; ------------------------------------------------------------
;; Depth First Search:

;; Depth first travel of graph with starting node as "node". "ifn" is
;; executed at node entry and "ofn" is executed at node
;; exit. Functions can return 'exit, 'stop, or 'continue as orders for
;; defining the next steps.
;;
;; Args:
;;     node: Node.
;;     ifn:  Node input action.
;;     ofn:  Node output action.
;;     env:  Action environment.
;;
;; Example:
;;     (dfs-common node (lambda (node env) (displayln (node-name node))) #f #f)
;;
(define (dfs-common node ifn ofn env)
  (when node

    (let ((visited empty))

      (define (visit node)
        (node-visit node)
        (set! visited (cons node visited)))

      (call/cc
       (lambda (exit-cc)
         (define (dfs+ exit-cc node ifn ofn env)
           (call/cc
            (lambda (stop-cc)
              (when ifn
                (case (ifn node env)
                  ((exit) (exit-cc #f))
                  ((stop) (stop-cc #f))))
              (let each-olink ((olink (node-olink node)))
                (when (pair? olink)
                  (when (not (node-visited? (edge-node (car olink))))
                    (dfs+ exit-cc (edge-node (car olink)) ifn ofn env))
                  (visit (edge-node (car olink)))
                  (each-olink (cdr olink))))
              (when ofn
                (case (ofn node env)
                  ((exit) (exit-cc #f))
                  ((stop) (stop-cc #f)))))))
         (dfs+ exit-cc node ifn ofn env)))
      (node-unvisit-all visited))))

;; Depth-first-search from "node" with action "fn" on input.
(define (dfs node fn)
  (dfs-common node fn #f #f))

;; Depth-first-search from "node" with action "fn" on input with
;; "env".
(define (dfs-with-env node fn env)
  (dfs-common node fn #f env))


;; ------------------------------------------------------------
;; Breath First Search:

;; Breath first travel of graph with starting node as "node".
;;
;; Args:
;;     node: Node.
;;     ifn:  Node input action.
;;     ofn:  Node output action.
;;     env:  Action environment.
;;
;; Example:
;;     (bfs-common node (lambda (node) (displayln (node-name node))) #f #f)
;;
(define (bfs-common node ifn ofn env)
  (when node

    (let ((visited empty))

      (define (visit node)
        (node-visit node)
        (set! visited (cons node visited)))

      (call/cc
       (lambda (exit-cc)

         ;; Process nodes and build list of next-nodes for the next
         ;; round of bfs+.
         (define (bfs+ exit-cc nodes ifn ofn env)
           (let ((next-nodes empty))

             (let each-node ((nodes nodes))
               (when (pair? nodes)

                 ;; Process all output links.
                 (let each-olink ((olink (node-olink (car nodes))))
                   (when (pair? olink)
                     (let ((probe-node (edge-node (car olink))))
                       (when (not (node-visited? probe-node))
                         (visit probe-node)
                         (call/cc
                          (lambda (stop-cc)
                            (when ifn
                              (case (ifn probe-node env)
                                ((exit) (exit-cc #f))
                                ((stop) (stop-cc #f))))
                            (set! next-nodes
                              (append next-nodes (list probe-node)))))))
                     (each-olink (cdr olink))))

                 (when ofn
                   (case (ofn (car nodes) env)
                     ((exit) (exit-cc #f))))
                 (each-node (cdr nodes))))

             (when (pair? next-nodes)
               (bfs+ exit-cc next-nodes ifn ofn env))))

         ;; Visit first node as special case.
         (when ifn
           (case (ifn node env)
             ((exit stop) (exit-cc #f))))
         (visit node)
         (bfs+ exit-cc (list node) ifn ofn env)))

      ;; Clean-up before returning.
      (node-unvisit-all visited))))


;; Breath-first-search from "node" with action "fn" on input.
(define (bfs node fn)
  (bfs-common node fn #f #f))

;; Breath-first-search from "node" with action "fn" on input with
;; "env".
(define (bfs-with-env node fn env)
  (bfs-common node fn #f env))


;; ------------------------------------------------------------
;; Topological sort:

(define (topological-sort node)

  ;; Input function.
  (define (topo-sort-ifn node env)
    (if (not (node-visited? node))
        (begin
          (node-visit node)
          'continue)
        'stop))

  ;; Output function.
  (define (topo-sort-ofn node env)
    (set-box! env (cons node (unbox env))))

  ;; DFS based ordering using stack.
  (let ((ordered (box empty)))
    (dfs-common node topo-sort-ifn topo-sort-ofn ordered)
    (for ((node (unbox ordered)))
      (node-unvisit node))
    (unbox ordered)))


;; ------------------------------------------------------------
;; Shortest and longest path:

;; Return shortest or longest path depending on "mode". Edge values
;; define the distance.
;;
;; Return list of (node . length) pairs looking from "a".
(define (shortest-or-longest-path-common a mode) ; INTERNAL

  (let ((inf #f)
        (compare #f))

    (case mode
      ((shortest-path)
       (set! inf +inf.0)
       (set! compare >))
      ((longest-path)
       (set! inf -inf.0)
       (set! compare <)))

    (define (get-distance node)
      (unbox (car (node-data node))))

    (define (set-distance! node val)
      (set-box! (car (node-data node)) val))

    ;; Mark distances for sorted list of nodes.
    (let ((sorted-nodes (topological-sort a)))

      ;; Set all nodes to infinity.
      (for ((node sorted-nodes))
        (set-node-data! node (cons (box inf) (node-data node))))
      (set-distance! a 0)

      (let each-node ((sorted sorted-nodes))
        (when (pair? sorted)
          (let each-olink ((olink (node-olink (car sorted))))
            (when (pair? olink)
              (let ((node (edge-node (car olink)))
                    (new-value (+ (get-distance (car sorted)) (edge-data (car olink)))))
                (when (compare (get-distance node) new-value)
                  (set-distance! node new-value)))
              (each-olink (cdr olink))))
          (each-node (cdr sorted))))

      (let ((dist (map
                   (lambda (i)
                     (cons i (get-distance i)))
                   sorted-nodes)))
        (for ((node sorted-nodes))
          (set-node-data! node (cdr (node-data node))))
        dist))))


;; Return shortest distance between nodes "a" and "b". Edge values
;; define the distance.
(define (shortest-path-between-nodes a b)
  (let ((dist (shortest-or-longest-path-common a 'shortest-path)))
    (cdr
     (findf
      (lambda (i) (eqv? b (car i)))
      dist))))


;; Return longest distance between nodes "a" and "b". Edge values
;; define the distance.
(define (longest-path-between-nodes a b)
  (let ((dist (shortest-or-longest-path-common a 'longest-path)))
    (cdr
     (findf
      (lambda (i) (eqv? b (car i)))
      dist))))

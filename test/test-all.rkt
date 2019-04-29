#lang racket

(require rackunit)
(require "../graph.rkt")

(define test-result empty)

(define (collect-node node env)
  (set-box! env (append (unbox env) (list node)))
  'continue)

(define (get-ids nodes)
  (map
   (lambda (node)
     (node-name node))
   nodes))

(define (run-test fn)
  (set! test-result (box empty))
  (fn)
  (get-ids (unbox test-result)))


(test-case "test-basic"

  (define g (new graph% (name: "test-graph")))

  ;;
  ;;
  ;;             .-task2----.
  ;;     task1--<            >--task4
  ;;             '-task3----'
  ;;
  ;;

  (send g add-with-data
        'task1 10
        'task2 8
        'task3 5
        'task4 10)

  (send g connect
        'task1 'task2
        'task1 'task3
        'task2 'task4
        'task3 'task4)

  (check-equal? (run-test
                 (lambda () (dfs-with-env (send g ref 'task1) collect-node test-result)))
                '(task1 task2 task4 task3)
                "dfs-with-env")
  (check-equal? (run-test
                 (lambda () (bfs-with-env (send g ref 'task1) collect-node test-result)))
                '(task1 task2 task3 task4)
                "bfs-with-env")
  
  )

(test-case "test-complex"

  (define g (new graph% (name: "test-graph")))
  
  ;;
  ;;               2      3
  ;;             .----T2----.        3
  ;;     T1-----<  2      2  >----T4-------T7
  ;;             '----T3----'         .'
  ;;                '-----T5--T6-----'
  ;;                    6    4   7
  ;;

  (send g add-with-data
        't1 12
        't2 8
        't3 5
        't4 10
        't5 10
        't6 10
        't7 10)

  (send g connect-with-data
        't1 't2 2
        't1 't3 2
        't2 't4 3
        't3 't4 2
        't4 't7 3
        't1 't5 6
        't5 't6 4
        't6 't7 7
        )

  (check-equal? (run-test
                 (lambda () (dfs-with-env (send g ref 't1) collect-node test-result)))
                '(t1 t2 t4 t7 t3 t5 t6)
                "dfs-with-env")

  (check-equal? (run-test
                 (lambda () (bfs-with-env (send g ref 't1) collect-node test-result)))
                '(t1 t2 t3 t5 t4 t6 t7)
                "bfs-with-env")

  ;; Run twice to ensure no garbage state is disturbing the next
  ;; round of toposort.
  (for ((i 2))
    (check-equal? (get-ids (topological-sort (send g first-node)))
                  '(t1 t5 t6 t3 t2 t4 t7)
                  "topological-sort"))

  (check-equal? (run-test
                 (lambda () (dfs-with-env (send g ref 't1) collect-node test-result)))
                '(t1 t2 t4 t7 t3 t5 t6)
                "dfs-with-env")

  (check-equal? (shortest-path-between-nodes (send g ref 't1) (send g ref 't7))
                7
                "shortest-path-between-nodes")

  (check-equal? (longest-path-between-nodes (send g ref 't1) (send g ref 't7))
                17
                "longest-path-between-nodes")

  )

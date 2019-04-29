#lang info

(define collection "ti-graph")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
;;(define scribblings '(("scribblings/ti-graph.scrbl" ())))
(define pkg-desc "Graph library (by TI)")
(define version "0.1")
(define pkg-authors '(Tero Isannainen))
(define compile-omit-paths '("test"))

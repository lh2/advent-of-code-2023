(defpackage #:aoc-test/day-17
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-17)

(define-test test-day-17
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 17 "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")
    (assert= 102 task-1)
    (assert= 94 task-2)))

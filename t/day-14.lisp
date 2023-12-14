(defpackage #:aoc-test/day-14
  (:use #:cl #:lisp-unit2 #:aoc/day-14))
(in-package #:aoc-test/day-14)

(define-test test-day-14
    ()
  (let ((*minimum-pattern-length* 5))
    (multiple-value-bind (task-1 task-2)
        (aoc:run-day 14 "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")
      (assert= 136 task-1)
      (assert= 64 task-2))))

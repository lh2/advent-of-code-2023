(defpackage #:aoc-test/day-14
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-14)

(define-test test-day-14
    ()
  (multiple-value-bind (task-1)
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
    (assert= 136 task-1)))

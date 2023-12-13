(defpackage #:aoc-test/day-13
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-13)

(define-test test-day-13
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 13 "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")
    (assert= 405 task-1)))

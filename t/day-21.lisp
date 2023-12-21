(defpackage #:aoc-test/day-21
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-21)

(define-test test-day-21
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 21 "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........." 6)
    (assert= 16 task-1)))

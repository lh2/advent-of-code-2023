(defpackage #:aoc-test/day-3
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-3)

(define-test test-day-3
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 3 "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")
    (assert= 4361 task-1)
    (assert= 467835 task-2)))

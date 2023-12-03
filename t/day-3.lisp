(defpackage #:aoc-test/day-3
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-3)

(define-test test-day-3
    ()
  (assert= 4361
           (aoc:run-day 3 "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")))

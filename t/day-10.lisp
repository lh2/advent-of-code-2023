(defpackage #:aoc-test/day-10
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-10)

(define-test test-day-10
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 10 ".....
.S-7.
.|.|.
.L-J.
.....")
    (assert= 4 task-1))
  (multiple-value-bind (task-1)
      (aoc:run-day 10 "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")
    (assert= 8 task-1)))

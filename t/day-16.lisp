(defpackage #:aoc-test/day-16
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-16)

(define-test test-day-16
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 16 ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")
    (assert= 46 task-1)))

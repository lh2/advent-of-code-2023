(defpackage #:aoc-test/day-9
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-9)

(define-test test-day-9
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 9 "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")
    (assert= 114 task-1)
    (assert= 2 task-2)))

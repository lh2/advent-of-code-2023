(defpackage #:aoc-test/day-6
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-6)

(define-test test-day-6
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 6 "Time:      7  15   30
Distance:  9  40  200")
    (assert= 288 task-1)
    (assert= 71503 task-2)))

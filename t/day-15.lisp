(defpackage #:aoc-test/day-15
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-15)

(define-test test-day-15
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 15 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
    (assert= 1320 task-1)))

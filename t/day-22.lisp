(defpackage #:aoc-test/day-22
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-22)

(define-test test-day-22
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 22 "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")
    (assert= 5 task-1)))

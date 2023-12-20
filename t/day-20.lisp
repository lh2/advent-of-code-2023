(defpackage #:aoc-test/day-20
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-20)

(define-test test-day-20
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 20 "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")
    (assert= 32000000 task-1))
  (multiple-value-bind (task-1)
      (aoc:run-day 20 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")
    (assert= 11687500 task-1)))

(defpackage #:aoc-test/day-8
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-8)

(define-test test-day-8
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 8 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")
    (assert= 2 task-1))
  (multiple-value-bind (task-1)
      (aoc:run-day 8 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")
    (assert= 6 task-1)))

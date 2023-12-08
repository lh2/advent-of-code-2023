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
    (assert= 6 task-1))

  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 8 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")
    (declare (ignore task-1))
    (assert= 6 task-2)))

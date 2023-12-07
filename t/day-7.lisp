(defpackage #:aoc-test/day-7
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-7)

(define-test test-day-7
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 7 "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")
    (assert= 6440 task-1)))

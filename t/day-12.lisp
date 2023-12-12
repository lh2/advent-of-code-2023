(defpackage #:aoc-test/day-12
  (:use #:cl #:lisp-unit2 #:aoc/day-12))
(in-package #:aoc-test/day-12)

(define-test test-day-12
    ()
  (assert= 1 (possible-arrangements "???.###" (list 1 1 3)))
  (assert= 10 (possible-arrangements "?###????????" (list 3 2 1))))

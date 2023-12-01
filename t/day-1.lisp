(defpackage #:aoc-test/day-1
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-1)

(define-test test-day-1
    ()
  (assert= 142
           (aoc:run-day 1 "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"))

  (assert= 281
           (nth-value 1 (aoc:run-day 1 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"))))

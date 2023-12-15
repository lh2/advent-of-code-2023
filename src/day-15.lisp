(defpackage #:aoc/day-15
  (:use #:cl #:aoc/utils)
  (:export #:day-15))
(in-package #:aoc/day-15)

(defun day-15 (input)
  (loop with sum = 0
        with current = 0
        for char = (read-char input nil)
        while char
        do (cond
             ((char= char #\,)
              (incf sum current)
              (setf current 0))
             ((char= char #\Newline))
             (t
              (incf current (char-code char))
              (setf current (* current 17))
              (setf current (rem current 256))))
        finally (return (+ sum current))))

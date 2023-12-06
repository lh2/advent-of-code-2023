(defpackage #:aoc/day-6
  (:use #:cl #:aoc/utils)
  (:export #:day-6))
(in-package #:aoc/day-6)

(defun day-6 (input)
  (let* ((line (read-line input))
         (times (read-number-list line :start (1+ (position #\: line))))
         (line (read-line input))
         (distances (read-number-list line :start (1+ (position #\: line)))))
    (loop with task-1 = nil
          for time in times
          for distance in distances
          do (push (loop for speed from 1 below time
                         for remaining-time = (- time speed)
                         for travelled = (* speed remaining-time)
                         when (> travelled distance)
                           sum 1 fixnum)
                   task-1)
          finally (return (apply #'* task-1)))))

(defpackage #:aoc/day-8
  (:use #:cl #:aoc/utils)
  (:export #:day-8))
(in-package #:aoc/day-8)

(defun parse-line (line)
  (let* ((equal-pos (position #\= line))
         (key (subseq line 0 (1- equal-pos)))
         (paren-open-pos (position #\( line :start equal-pos))
         (comma-pos (position #\, line :start paren-open-pos))
         (left (subseq line (1+ paren-open-pos) comma-pos))
         (paren-close-pos (position #\) line :start comma-pos))
         (right (subseq line (+ comma-pos 2) paren-close-pos)))
    (list key (list left right))))

(defun read-map (input)
  (read-line input)
  (loop with map = (make-hash-table :test 'equal)
        for line = (read-line input nil)
        while line
        for (key left-right) = (parse-line line)
        do (setf (gethash key map) left-right)
        finally (return map)))

(defun day-8 (input)
  (let ((instructions (read-line input))
        (map (read-map input)))
    (loop with current = "AAA"
          for steps from 0
          for direction = (aref instructions (mod steps (length instructions)))
          for (left right) = (gethash current map)
          do (setf current
                   (ecase direction
                     (#\L left)
                     (#\R right)))
          when (equal current "ZZZ")
            do (return (1+ steps)))))

(defpackage #:aoc/day-4
  (:use #:cl #:aoc/utils)
  (:export #:day-4))
(in-package #:aoc/day-4)

(defun read-card-numbers (line)
  (loop with winning-numbers = nil
        with my-numbers = nil
        with divider? = nil
        for i from 8 below (length line)
        for char = (aref line i)
        do (cond
             ((char= char #\Space))
             ((char= char #\|)
              (setf divider? t))
             ((digit-char-p char)
              (multiple-value-bind (number end)
                  (parse-integer line
                                 :start i
                                 :junk-allowed t)
                (if divider?
                    (push number my-numbers)
                    (push number winning-numbers))
                (setf i end))))
        finally (return (list winning-numbers my-numbers))))

(declaim (ftype (function (list list) fixnum) task-1))
(defun task-1 (winning-numbers my-numbers)
  (loop with score fixnum = 0
        for number fixnum in my-numbers
        when (member number winning-numbers)
          do (setf score (if (= score 0)
                             1
                             (* score 2)))
        finally (return score)))

(defun day-4 (input)
  (loop for line = (read-line input nil)
        while line
        for (winning-numbers my-numbers) = (read-card-numbers line)
        sum (task-1 winning-numbers my-numbers) into task-1 fixnum
        finally (return (values task-1))))

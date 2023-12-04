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

(declaim (ftype (function (list list) fixnum) card-matching-numbers))
(defun card-matching-numbers (winning-numbers my-numbers)
  (loop for number fixnum in my-numbers
        when (member number winning-numbers)
          sum 1 fixnum))

(declaim (ftype (function (list) (values fixnum list)) process-copies))
(defun process-copies (copies)
  (loop with total-copies fixnum = 0
        with new-copies = nil
        for copy fixnum in copies
        do (decf copy)
        when (>= copy 0)
          do (incf total-copies)
        when (> copy 0)
          do (push copy new-copies)
        finally (return (values total-copies new-copies))))

(defun day-4 (input)
  (loop with copies = nil
        with task-2 fixnum = 0
        for line = (read-line input nil)
        while line
        for (winning-numbers my-numbers) = (read-card-numbers line)
        for matching-numbers = (card-matching-numbers winning-numbers my-numbers)
        do (multiple-value-bind (this-card-copies new-copies)
               (process-copies copies)
             (incf task-2 (1+ this-card-copies))
             (setf copies new-copies)
             (loop for copy from 0 to this-card-copies
                   do (push matching-numbers copies)))
        sum (task-1 winning-numbers my-numbers) into task-1 fixnum
        finally (return (values task-1 task-2))))

(defpackage #:aoc/day-9
  (:use #:cl #:aoc/utils)
  (:export #:day-9))
(in-package #:aoc/day-9)

(defun diff-sequence (numbers)
  (loop with last = (first numbers)
        for number in (rest numbers)
        collect (- number last)
        do (setf last number)))

(defun find-next-value (numbers)
  (let ((new-sequence (diff-sequence numbers)))
    (if (every (curry #'= 0) new-sequence)
        (first numbers)
        (+ (lastcar numbers)
           (find-next-value new-sequence)))))

(defun find-previous-value (numbers)
  (let ((new-sequence (diff-sequence numbers)))
    (if (every (curry #'= 0) new-sequence)
        (first numbers)
        (- (first numbers)
           (find-previous-value new-sequence)))))

(defun day-9 (input)
  (loop for line = (read-line input nil)
        while line
        for numbers = (read-number-list line)
        sum (find-next-value numbers) into task-1 fixnum
        sum (find-previous-value numbers) into task-2 fixnum
        finally (return (values task-1 task-2))))

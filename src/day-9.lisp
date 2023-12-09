(defpackage #:aoc/day-9
  (:use #:cl #:aoc/utils)
  (:export #:day-9))
(in-package #:aoc/day-9)

(defun day-9 (input)
  (loop with task-1 fixnum = 0
        with task-2 fixnum = 0
        for line = (read-line input nil)
        while line
        for numbers = (read-number-list line)
        do (loop with sequences = (list (cons (car numbers)
                                              (lastcar numbers)))
                 with current = numbers
                 with current-last = 0
                 for non-zero? = nil
                 do (setf current
                          (loop with last fixnum = (first current)
                                for number fixnum in (rest current)
                                for n = (the fixnum (- number last))
                                unless (xor non-zero?
                                            (zerop n))
                                  do (setf non-zero? t)
                                collect n
                                do (setf last number)
                                finally (setf current-last n)))
                 do (push (cons (car current)
                                current-last)
                          sequences)
                 while non-zero?
                 finally (progn
                           (loop with next-1 fixnum = 0
                                 with next-2 fixnum = 0
                                 for current in (rest sequences)
                                 do (setf next-1 (+ (the fixnum (cdr current)) next-1)
                                          next-2 (- (the fixnum (car current)) next-2))
                                 finally (progn
                                           (incf task-1 next-1)
                                           (incf task-2 next-2)))))
        finally (return (values task-1 task-2))))

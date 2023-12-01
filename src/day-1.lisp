(defpackage #:aoc/day-1
  (:use #:cl #:aoc/utils)
  (:export #:day-1))
(in-package #:aoc/day-1)

(defun first-digit (line)
  (loop for char across line
        when (digit-char-p char)
          do (return (char-number char))))

(defun last-digit (line)
  (loop for i from (1- (length line)) downto 0
        for char = (aref line i)
        when (digit-char-p char)
          do (return (char-number char))))

(defparameter *alpha-digit-table* '(("one" . 1)
                                    ("two" . 2)
                                    ("three" . 3)
                                    ("four" . 4)
                                    ("five" . 5)
                                    ("six" . 6)
                                    ("seven" . 7)
                                    ("eight" . 8)
                                    ("nine" . 9)))

(defun alpha-digit-at (line start)
  (loop for (compare . digit) in *alpha-digit-table*
        when (loop for i from start below (min (length line)
                                               (+ start (length compare)))
                   for ci from 0
                   always (char= (aref line i)
                                 (aref compare ci)))
          do (return digit)))

(defun reverse-alpha-digit-at (line end)
  (loop for (compare . digit) in *alpha-digit-table*
        for compare-length = (1- (length compare))
        when (loop for i from end downto (max (- end compare-length) 0)
                   for ci downfrom compare-length
                   always (char= (aref line i)
                                 (aref compare ci)))
          do (return digit)))

(defun first-digit-alpha (line)
  (loop for i from 0 below (length line)
        for char = (aref line i)
        when (digit-char-p char)
          do (return (char-number char))
        thereis (alpha-digit-at line i)))

(defun last-digit-alpha (line)
  (loop for i from (1- (length line)) downto 0
        for char = (aref line i)
        when (digit-char-p char)
          do (return (char-number char))
        thereis (reverse-alpha-digit-at line i)))

(defun day-1 (input)
  (loop for line in (read-input input)
        while line
        sum (+ (* (or (first-digit line) 0) 10)
               (or (last-digit line) 0)) into task-1
        sum (+ (* (first-digit-alpha line) 10)
               (last-digit-alpha line)) into task-2
        finally (return (values task-1 task-2))))

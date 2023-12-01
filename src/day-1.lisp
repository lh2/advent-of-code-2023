(defpackage #:aoc/day-1
  (:use #:cl #:aoc/utils)
  (:export #:day-1))
(in-package #:aoc/day-1)

#+release (declaim (inline first-digit last-digit
                           alpha-digit-at reverse-alpha-digit-at
                           first-digit-alpha last-digit-alpha))

(declaim (ftype (function (simple-string) fixnum) first-digit))
(defun first-digit (line)
  (or (loop for char across line
            when (digit-char-p char)
              do (return (char-number char)))
      0))

(declaim (ftype (function (simple-string) fixnum) last-digit))
(defun last-digit (line)
  (or (loop for i from (1- (length line)) downto 0
            for char = (aref line i)
            when (digit-char-p char)
              do (return (char-number char)))
      0))

(defparameter *alpha-digit-table* '(("one" . 1)
                                    ("two" . 2)
                                    ("three" . 3)
                                    ("four" . 4)
                                    ("five" . 5)
                                    ("six" . 6)
                                    ("seven" . 7)
                                    ("eight" . 8)
                                    ("nine" . 9)))

(declaim (ftype (function (simple-string fixnum) (or fixnum null)) alpha-digit-at))
(defun alpha-digit-at (line start)
  (loop for (compare . digit) (simple-string . fixnum) in *alpha-digit-table*
        when (loop for i from start below (min (length line)
                                               (+ start (length compare)))
                   for ci from 0
                   always (char= (aref line i)
                                 (aref compare ci)))
          do (return digit)))

(declaim (ftype (function (simple-string fixnum) (or fixnum null)) reverse-alpha-digit-at))
(defun reverse-alpha-digit-at (line end)
  (loop for (compare . digit) (simple-string . fixnum) in *alpha-digit-table*
        for compare-length = (1- (length compare))
        when (loop for i from end downto (max (- end compare-length) 0)
                   for ci downfrom compare-length
                   always (char= (aref line i)
                                 (aref compare ci)))
          do (return digit)))

(declaim (ftype (function (simple-string) fixnum) first-digit-alpha))
(defun first-digit-alpha (line)
  (or (loop for i from 0 below (length line)
            for char = (aref line i)
            when (digit-char-p char)
              do (return (char-number char))
            thereis (alpha-digit-at line i))
      0))

(declaim (ftype (function (simple-string) fixnum) last-digit-alpha))
(defun last-digit-alpha (line)
  (or (loop for i from (1- (length line)) downto 0
            for char = (aref line i)
            when (digit-char-p char)
              do (return (char-number char))
            thereis (reverse-alpha-digit-at line i))
      0))

(defun day-1 (input)
  (loop for line = (read-line input nil)
        while line
        sum (+ (the fixnum (* (first-digit line) 10))
               (last-digit line)) into task-1 fixnum
        sum (+ (the fixnum (* (first-digit-alpha line) 10))
               (last-digit-alpha line)) into task-2 fixnum
        finally (return (values task-1 task-2))))

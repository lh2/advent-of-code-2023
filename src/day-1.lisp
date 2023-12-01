(defpackage #:aoc/day-1
  (:use #:cl #:aoc/utils)
  (:export #:day-1))
(in-package #:aoc/day-1)

(defun first-digit (line)
  (loop for char across line
        when (digit-char-p char)
          do (return (char-number char))))

(defun last-digit (line)
  (loop for i from (- (length line) 1) downto 0
        for char = (aref line i)
        when (digit-char-p char)
          do (return (char-number char))))

(defun day-1 (input)
  (loop for line in (read-input input)
        while line
        sum (parse-integer (format nil "~A~A"
                                   (first-digit line)
                                   (last-digit line)))))

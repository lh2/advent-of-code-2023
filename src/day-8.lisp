(defpackage #:aoc/day-8
  (:use #:cl #:aoc/utils)
  (:export #:day-8))
(in-package #:aoc/day-8)

(declaim (ftype (function (simple-string) fixnum) convert-field-to-number))
(defun convert-field-to-number (field)
  (loop for char across field
        for shift fixnum from 0
        sum (the fixnum (ash (- (char-code char) (if (char>= char #\A) 65 48))
                             (the fixnum (* shift 5)))) fixnum))

(declaim (ftype (function (simple-string) list) parse-line))
(defun parse-line (line)
  (let* ((equal-pos (position #\= line))
         (key (subseq line 0 (1- equal-pos)))
         (paren-open-pos (position #\( line :start equal-pos))
         (comma-pos (position #\, line :start paren-open-pos))
         (left (subseq line (1+ paren-open-pos) comma-pos))
         (paren-close-pos (position #\) line :start comma-pos))
         (right (subseq line (+ comma-pos 2) paren-close-pos)))
    (list (convert-field-to-number key)
          (list (convert-field-to-number left)
                (convert-field-to-number right)))))

(declaim (ftype (function (stream) (simple-array list)) read-map))
(defun read-map (input)
  (read-line input)
  (loop with map = (make-array 26426 :initial-element nil)
        for line = (read-line input nil)
        while line
        for (key left-right) = (parse-line line)
        do (setf (aref map key) left-right)
        finally (return map)))

(declaim (ftype (function (simple-string
                           (simple-array list)
                           fixnum
                           &optional (or null fixnum))
                          fixnum)
                walk-to-destination))
(defun walk-to-destination (instructions map starting-point &optional destination)
  (loop with current fixnum = starting-point
        for steps fixnum from 0
        for direction = (aref instructions (mod steps (length instructions)))
        for (left right) = (aref map current)
        do (setf current
                 (ecase direction
                   (#\L left)
                   (#\R right)))
        when (if destination
                 (= current destination)
                 (>= current 25600))
          do (return (1+ steps))))

(defun day-8 (input)
  (let ((instructions (read-line input))
        (map (read-map input)))
    (values (if (aref map 0)
                (walk-to-destination instructions map 0 26425)
                0)
            (apply #'lcm
                   (loop for field fixnum from 0 to 825
                         when (aref map field)
                           collect (walk-to-destination instructions map field))))))

(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defun schematic-symbol-p (symbol)
  (and (char/= symbol #\.)
       (not (digit-char-p symbol))))

(defun extract-part-number (map y start-x)
  (loop for x from start-x below (array-dimension map 1)
        for char = (aref map y x)
        while (digit-char-p char)
        collect char into chars
        finally (return (values (parse-integer (coerce chars 'string))
                                (1- x)))))

(defun day-3 (input)
  (loop with map = (make-map input)
        for y from 0 below (array-dimension map 0)
        sum (loop with number-start = nil
                  for x from 0 below (array-dimension map 1)
                  for digit? = (digit-char-p (aref map y x))
                  when (and digit? (null number-start))
                    do (setf number-start x)
                  unless digit?
                    do (setf number-start nil)
                  when (and digit?
                            (member-if #'schematic-symbol-p
                                       (map-neighbours map y x)))
                    sum (multiple-value-bind (number next)
                            (extract-part-number map y number-start)
                          (setf x next)
                          number))))

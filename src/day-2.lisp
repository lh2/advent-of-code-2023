(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export #:day-2))
(in-package #:aoc/day-2)

(declaim (ftype (function (simple-string fixnum) list) parse-sets))
(defun parse-sets (line start)
  (loop with sets = nil
        with red = 0
        with green = 0
        with blue = 0
        with length = (length line)
        for pos from start
        do (multiple-value-bind (n end)
               (parse-integer line :start pos :junk-allowed t)
             (setf pos end)
             (ecase (aref line (1+ pos))
               (#\r (setf red n
                          pos (+ pos 4)))
               (#\g (setf green n
                          pos (+ pos 6)))
               (#\b (setf blue n
                          pos (+ pos 5)))))
        while (< pos length)
        when (char= (aref line pos) #\;)
          do (push (list red green blue) sets)
          and do (setf red 0 green 0 blue 0)
        finally (return (cons (list red green blue) sets))))

(declaim (ftype (function (simple-string) list) parse-game-line))
(defun parse-game-line (line)
  (multiple-value-bind (game-id colon-pos)
      (parse-integer line :start 5 :junk-allowed t)
    (list game-id
          (parse-sets line (1+ colon-pos)))))

(defparameter *minimum-cubes* (list 12 13 14))

(defun game-possible-p (sets)
  (loop for set in sets
        always (loop for color-1 fixnum in set
                     for color-2 fixnum in *minimum-cubes*
                     always (<= color-1 color-2))))

(declaim (ftype (function (list) fixnum) game-power))
(defun game-power (sets)
  (apply #'* (apply (curry #'mapcar #'max) sets)))

(defun day-2 (input)
  (loop for line = (read-line input nil)
        while line
        for (game sets) (fixnum list) = (parse-game-line line)
        when (game-possible-p sets)
          sum game into task-1 fixnum
        sum (game-power sets) into task-2 fixnum
        finally (return (values task-1 task-2))))

(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export #:day-2))
(in-package #:aoc/day-2)

(defun parse-sets (line start)
  (loop with sets = nil
        with red = 0
        with green = 0
        with blue = 0
        for pos from start below (length line)
        do (multiple-value-bind (n end)
               (parse-integer line :start pos :junk-allowed t)
             (setf pos end)
             (setf end (position-if (lambda (char)
                                      (or (char= #\, char)
                                          (char= #\; char)))
                                    line
                                    :start pos))
             (ecase (aref line (1+ pos))
               (#\r (setf red n))
               (#\g (setf green n))
               (#\b (setf blue n)))
             (setf pos (or end (1- (length line)))))
        when (char= (aref line pos) #\;)
          do (push (list red green blue) sets)
          and do (setf red 0 green 0 blue 0)
        finally (return (cons (list red green blue) sets))))

(defun parse-game-line (line)
  (let* ((start-game-id 5)
         (end-game-id (position #\: line)))
    (list (parse-integer line :start start-game-id :end end-game-id)
          (parse-sets line (1+ end-game-id)))))

(defparameter *minimum-cubes* (list 12 13 14))

(defun game-possible-p (sets)
  (loop for set in sets
        always (loop for color-1 in set
                     for color-2 in *minimum-cubes*
                     always (<= color-1 color-2))))

(defun game-power (sets)
  (apply #'*
         (apply #'mapcar
                (cons (lambda (&rest values)
                        (apply #'max values))
                      sets))))

(defun day-2 (input)
  (loop for line = (read-line input nil)
        while line
        for (game sets) = (parse-game-line line)
        when (game-possible-p sets)
          sum game into task-1
        sum (game-power sets) into task-2
        finally (return (values task-1 task-2))))

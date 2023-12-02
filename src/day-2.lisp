(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export #:day-2))
(in-package #:aoc/day-2)

(defun parse-sets (line start)
  (loop with sets = nil
        with current-set = nil
        for pos from start below (length line)
        do (multiple-value-bind (n end)
               (parse-integer line :start pos :junk-allowed t)
             (setf pos end)
             (setf end (position-if (lambda (char)
                                      (or (char= #\, char)
                                          (char= #\; char)))
                                    line
                                    :start pos))
             (setf (getf current-set (make-keyword (upcase (subseq line (1+ pos) end)))) n
                   pos (or end (1- (length line)))))
        when (char= (aref line pos) #\;)
          do (push current-set sets)
          and do (setf current-set nil)
        finally (return (cons current-set sets))))

(defun parse-game-line (line)
  (let* ((start-game-id 5)
         (end-game-id (position #\: line)))
    (list (parse-integer line :start start-game-id :end end-game-id)
          (parse-sets line (1+ end-game-id)))))

(defun game-possible-p (sets)
  (loop for set in sets
        for red = (getf set :red)
        for green = (getf set :green)
        for blue = (getf set :blue)
        always (and (or (null red)
                        (<= red 12))
                    (or (null green)
                        (<= green 13))
                    (or (null blue)
                        (<= blue 14)))))

(defun game-power (sets)
  (apply #'*
         (mapcar (lambda (color)
                   (apply #'max (cons 0 (remove nil (mapcar (rcurry #'getf color) sets)))))
                 (list :red :green :blue))))

(defun day-2 (input)
  (loop for line = (read-line input nil)
        while line
        for (game sets) = (parse-game-line line)
        when (game-possible-p sets)
          sum game into task-1
        sum (game-power sets) into task-2
        finally (return (values task-1 task-2))))

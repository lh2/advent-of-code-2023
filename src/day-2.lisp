(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export #:day-2))
(in-package #:aoc/day-2)

(defun parse-game-line (line)
  (let* ((game-cubes (str:split ": " line))
         (game (parse-integer (subseq (first game-cubes) 5)))
         (sets (mapcar (lambda (set)
                         (let ((cubes (str:split ", " set)))
                           (mapcan (lambda (cube-color)
                                     (let ((cube-color (str:split " " cube-color)))
                                       (list (make-keyword (string-upcase (second cube-color)))
                                             (parse-integer (first cube-color)))))
                                   cubes)))
                       (str:split "; " (second game-cubes)))))
    (list game sets)))

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

(defun day-2 (input)
  (loop for line = (read-line input nil)
        while line
        for (game sets) = (parse-game-line line)
        when (game-possible-p sets)
          sum game into task-1
        finally (return (values task-1))))

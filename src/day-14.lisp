(defpackage #:aoc/day-14
  (:use #:cl #:aoc/utils)
  (:export #:day-14))
(in-package #:aoc/day-14)

(defun slide-rock (map pos)
  (loop for y from (1- (point-y pos)) downto 0
        for cell = (map-cell map (cons (point-x pos) y))
        while (char= cell #\.)
        finally (setf (map-cell map pos) #\.
                      (map-cell map (cons (point-x pos) (1+ y))) #\O)))

(defun slide-rocks (map)
  (loop for y from 0 below (input-map-height map)
        do (loop for x from 0 below (input-map-height map)
                 for pos = (cons x y)
                 for cell = (map-cell map pos)
                 when (char= cell #\O)
                   do (slide-rock map pos))))

(defun total-load (map)
  (loop for y from 0 below (input-map-height map)
        for load-multiplier downfrom (input-map-height map)
        sum (loop for x from 0 below (input-map-width map)
                  when (char= (map-cell map (cons x y)) #\O)
                    sum load-multiplier)))

(defun day-14 (input)
  (let ((map (make-map input)))
    (slide-rocks map)
    (total-load map)))

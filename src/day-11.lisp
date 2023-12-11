(defpackage #:aoc/day-11
  (:use #:cl #:aoc/utils)
  (:export #:day-11))
(in-package #:aoc/day-11)

(defun find-galaxies (map &optional (expansion-factor 2))
  (decf expansion-factor)
  (let (galaxies
        empty-rows
        empty-cols)
    (loop for x from 0 below (input-map-width map)
          for contains-galaxy? = nil
          do (loop for y from 0 below (input-map-height map)
                   when (char= (map-cell map (cons x y)) #\#)
                     do (setf contains-galaxy? t)
                     and do (push (cons x y) galaxies))
          unless contains-galaxy?
            do (push x empty-cols))
    (loop for y from 0 below (input-map-height map)
          unless (loop for x from 0 below (input-map-width map)
                       thereis (char= (map-cell map (cons x y)) #\#))
            do (push y empty-rows))
    (loop for galaxy in galaxies
          collect (cons (+ (point-x galaxy)
                           (* (count-if (curry #'> (point-x galaxy)) empty-cols)
                              expansion-factor))
                        (+ (point-y galaxy)
                           (* (count-if (curry #'> (point-y galaxy)) empty-rows)
                              expansion-factor))))))

(defun day-11 (input)
  (let* ((map (make-map input))
         (galaxies-1 (find-galaxies map))
         (galaxies-2 (find-galaxies map 1000000))
         (task-1 0)
         (task-2 0))
    (map-combinations (lambda (p)
                        (incf task-1 (apply #'manhattan-distance p)))
                      galaxies-1
                      :length 2)
    (map-combinations (lambda (p)
                        (incf task-2 (apply #'manhattan-distance p)))
                      galaxies-2
                      :length 2)
    (values task-1 task-2)))

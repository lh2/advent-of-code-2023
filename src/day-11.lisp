(defpackage #:aoc/day-11
  (:use #:cl #:aoc/utils)
  (:export #:day-11))
(in-package #:aoc/day-11)

(defun find-galaxies (map)
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
                           (count-if (curry #'> (point-x galaxy)) empty-cols))
                        (+ (point-y galaxy)
                           (count-if (curry #'> (point-y galaxy)) empty-rows))))))

(defun day-11 (input)
  (let* ((map (make-map input))
         (galaxies (find-galaxies map))
         (sum 0))
    (map-combinations (lambda (p)
                        (incf sum (apply #'manhattan-distance p)))
                      galaxies
                      :length 2)
    (values sum)))

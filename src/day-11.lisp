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
    (values galaxies empty-rows empty-cols)))

(defun expand-universe (galaxies empty-rows empty-cols expansion-factor)
  (decf expansion-factor)
  (loop for galaxy in galaxies
        collect (cons (+ (point-x galaxy)
                         (* (count-if (curry #'> (point-x galaxy)) empty-cols)
                            expansion-factor))
                      (+ (point-y galaxy)
                         (* (count-if (curry #'> (point-y galaxy)) empty-rows)
                            expansion-factor)))))

(defun sum-distances (galaxies)
  (loop for (galaxy-1 . rest) on galaxies
        while rest
        sum (loop for galaxy-2 in rest
                  sum (manhattan-distance galaxy-1 galaxy-2))))

(defun day-11 (input)
  (multiple-value-bind (galaxies empty-rows empty-cols)
      (find-galaxies (make-map input))
    (let ((galaxies-1 (expand-universe galaxies empty-rows empty-cols 2))
          (galaxies-2 (expand-universe galaxies empty-rows empty-cols 1000000)))
      (values (sum-distances galaxies-1)
              (sum-distances galaxies-2)))))

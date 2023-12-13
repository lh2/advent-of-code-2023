(defpackage #:aoc/day-13
  (:use #:cl #:aoc/utils)
  (:export #:day-13))
(in-package #:aoc/day-13)

(defun reflection-finding-properties (map type)
  (ecase type
    (:vertical
     (values (input-map-width map)
             (input-map-height map)
             (lambda (primary secondary)
               (cons primary secondary))))
    (:horizontal
     (values (input-map-height map)
             (input-map-width map)
             (lambda (primary secondary)
               (cons secondary primary))))))

(defun find-point-of-reflection (map type)
  (multiple-value-bind (primary-axis-length secondary-axis-length make-point)
      (reflection-finding-properties map type)
    (loop for reflection-point from 0 below (1- primary-axis-length)
          for reflection-length = (min (1+ reflection-point)
                                       (- primary-axis-length reflection-point 1))
          when (loop repeat reflection-length
                     for compare-1 downfrom reflection-point
                     for compare-2 from (1+ reflection-point)
                     always (loop for secondary-axis-point from 0 below secondary-axis-length
                                  for point-1 = (funcall make-point compare-1 secondary-axis-point)
                                  for point-2 = (funcall make-point compare-2 secondary-axis-point)
                                  always (char= (map-cell map point-1)
                                                (map-cell map point-2))))
            do (return (1+ reflection-point)))))

(defun day-13 (input)
  (loop for map = (make-map input)
        while map
        sum (or (find-point-of-reflection map :vertical)
                (* (find-point-of-reflection map :horizontal) 100))))

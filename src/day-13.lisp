(defpackage #:aoc/day-13
  (:use #:cl #:aoc/utils)
  (:export #:day-13))
(in-package #:aoc/day-13)

(defun find-vertical-point-of-reflection (map)
  (loop with width = (input-map-width map)
        with height = (input-map-height map)
        for reflection-x from 0 below (1- width)
        for reflection-length = (min (1+ reflection-x)
                                     (- width reflection-x 1))
        when (loop repeat reflection-length
                   for x-1 downfrom reflection-x
                   for x-2 from (1+ reflection-x)
                   always (loop for y from 0 below height
                                always (char= (map-cell map (cons x-1 y))
                                              (map-cell map (cons x-2 y)))))
          do (return (1+ reflection-x))))

(defun find-horizontal-point-of-reflection (map)
  (loop with width = (input-map-width map)
        with height = (input-map-height map)
        for reflection-y from 0 below (1- height)
        for reflection-length = (min (1+ reflection-y)
                                     (- height reflection-y 1))
        when (loop repeat reflection-length
                   for y-1 downfrom reflection-y
                   for y-2 from (1+ reflection-y)
                   always (loop for x from 0 below width
                                always (char= (map-cell map (cons x y-1))
                                              (map-cell map (cons x y-2)))))
          do (return (1+ reflection-y))))

(defun day-13 (input)
  (loop for map = (make-map input)
        while map
        sum (or (find-vertical-point-of-reflection map)
                (* (find-horizontal-point-of-reflection map) 100))))

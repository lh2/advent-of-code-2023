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

(defun find-reflection-imperfections (map reflection-point primary-axis-length secondary-axis-length make-point)
  (loop with reflection-length = (min (1+ reflection-point)
                                      (- primary-axis-length reflection-point 1))
        with imperfections = 0
        repeat reflection-length
        for compare-1 downfrom reflection-point
        for compare-2 from (1+ reflection-point)
        while (<= imperfections 1)
        do (loop for secondary-axis-point from 0 below secondary-axis-length
                 for point-1 = (funcall make-point compare-1 secondary-axis-point)
                 for point-2 = (funcall make-point compare-2 secondary-axis-point)
                 when (char/= (map-cell map point-1)
                              (map-cell map point-2))
                   do (incf imperfections)
                 while (<= imperfections 1))
        finally (return imperfections)))

(defun find-point-of-reflection (map type)
  (multiple-value-bind (primary-axis-length secondary-axis-length make-point)
      (reflection-finding-properties map type)
    (loop with perfect-point-of-reflection = nil
          with imperfect-point-of-reflection = nil
          for reflection-point from 0 below (1- primary-axis-length)
          for reflection-imperfections = (find-reflection-imperfections map reflection-point
                                                                        primary-axis-length secondary-axis-length
                                                                        make-point)
          when (= reflection-imperfections 0)
            do (setf perfect-point-of-reflection (1+ reflection-point))
          when (= reflection-imperfections 1)
            do (setf imperfect-point-of-reflection (1+ reflection-point))
          until (and perfect-point-of-reflection imperfect-point-of-reflection)
          finally (return (list perfect-point-of-reflection imperfect-point-of-reflection)))))

(defun day-13 (input)
  (loop for map = (make-map input)
        while map
        for (perfect-vertical-reflection imperfect-vertical-reflection) = (find-point-of-reflection map :vertical)
        for (perfect-horizontal-reflection imperfect-horizontal-reflection) = (find-point-of-reflection map :horizontal)
        sum (or perfect-vertical-reflection
                (* perfect-horizontal-reflection 100)) into task-1
        sum (or imperfect-vertical-reflection
                (* imperfect-horizontal-reflection 100)) into task-2
        finally (return (values task-1 task-2))))

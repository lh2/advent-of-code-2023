(defpackage #:aoc/day-14
  (:use #:cl #:aoc/utils)
  (:export
   #:day-14
   #:*minimum-pattern-length*))
(in-package #:aoc/day-14)

(defun point-x-constructor (y)
  (lambda (x)
    (cons x y)))

(defun point-y-constructor (x)
  (lambda (y)
    (cons x y)))

(defun make-in-range-p (to)
  (lambda (i)
    (and (>= i 0)
         (< i to))))

(defun slide-rock-direction-configuration (map point direction)
  (ecase direction
    (:north
     (values (1- (point-y point))
             #'1- #'1+
             (make-in-range-p (input-map-height map))
             (point-y-constructor (point-x point))))
    (:west
     (values (1- (point-x point))
             #'1- #'1+
             (make-in-range-p (input-map-width map))
             (point-x-constructor (point-y point))))
    (:south
     (values (1+ (point-y point))
             #'1+ #'1-
             (make-in-range-p (input-map-height map))
             (point-y-constructor (point-x point))))
    (:east
     (values (1+ (point-x point))
             #'1+ #'1-
             (make-in-range-p (input-map-width map))
             (point-x-constructor (point-y point))))))

(defun slide-rock (map pos direction)
  (multiple-value-bind (start-pos by final in-range-p make-point)
      (slide-rock-direction-configuration map pos direction)
    (loop for i = start-pos then (funcall by i)
          while (funcall in-range-p i)
          for cell = (map-cell map (funcall make-point i))
          while (char= cell #\.)
          finally (setf (map-cell map pos) #\.
                        (map-cell map (funcall make-point (funcall final i))) #\O))))

(defun slide-direction-configuration (map direction)
  (ecase direction
    (:north
     (values 0 #'1+
             (curry #'> (input-map-height map)) (input-map-width map)
             (lambda (d-1 d-2)
               (cons d-2 d-1))))
    (:west
     (values 0 #'1+
             (curry #'> (input-map-width map)) (input-map-height map)
             (lambda (d-1 d-2)
               (cons d-1 d-2))))
    (:south
     (values (1- (input-map-height map)) #'1-
             (curry #'<= 0) (input-map-width map)
             (lambda (d-1 d-2)
               (cons d-2 d-1))))
    (:east
     (values (1- (input-map-width map)) #'1-
             (curry #'<= 0) (input-map-height map)
             (lambda (d-1 d-2)
               (cons d-1 d-2))))))

(defun slide-rocks (map direction)
  (multiple-value-bind (d-1-from d-1-by d-1-while-p d-2-below make-point)
      (slide-direction-configuration map direction)
    (loop for d-1 = d-1-from then (funcall d-1-by d-1)
          while (funcall d-1-while-p d-1)
          do (loop for d-2 from 0 below d-2-below
                   for pos = (funcall make-point d-1 d-2)
                   for cell = (map-cell map pos)
                   when (char= cell #\O)
                     do (slide-rock map pos direction)))))

(defun total-load (map)
  (loop for y from 0 below (input-map-height map)
        for load-multiplier downfrom (input-map-height map)
        sum (loop for x from 0 below (input-map-width map)
                  when (char= (map-cell map (cons x y)) #\O)
                    sum load-multiplier)))

(defun spin-cycle (map &optional (cycle '(:north :west :south :east)))
  (loop for direction in cycle
        do (slide-rocks map direction)))

(defparameter *minimum-pattern-length* 100)

(defun day-14 (input)
  (let ((map (make-map input))
        task-1 task-2)
    (slide-rocks map :north)
    (setf task-1 (total-load map))
    (spin-cycle map '(:west :south :east))
    (loop with history = nil
          for cycles from 1
          do (spin-cycle map)
          do (push (total-load map) history)
          do (let ((pattern-length (find-pattern history *minimum-pattern-length*)))
               (when pattern-length
                 (setf task-2
                       (elt history
                            (1+ (- pattern-length
                                   (- (mod 1000000000 pattern-length)
                                      (- cycles (* pattern-length 2)))))))
                 (return))))
    (values task-1 task-2)))

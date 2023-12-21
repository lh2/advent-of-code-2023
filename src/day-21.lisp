(defpackage #:aoc/day-21
  (:use #:cl #:aoc/utils)
  (:export #:day-21))
(in-package #:aoc/day-21)

(defun find-start-position (map)
  (loop for y from 0 below (input-map-height map)
        do (loop for x from 0 below (input-map-width map)
                 for point = (cons x y)
                 when (char= (map-cell map point) #\S)
                   do (return-from find-start-position point))))

(defun neighbouring-positions (position)
  (destructuring-bind (x . y)
      position
    (let (ps)
      (push (cons (1- x) y) ps)
      (push (cons (1+ x) y) ps)
      (push (cons x (1- y)) ps)
      (push (cons x (1+ y)) ps)
      ps)))

(defun neighbouring-gardens (map width height position)
  (loop for n in (neighbouring-positions position)
        for cell = (map-cell map (destructuring-bind (x . y)
                                     n
                                   (cons (mod x width)
                                         (mod y height))))
        when (or (char= cell #\.)
                 (char= cell #\S))
          collect n))

(defun step-through-garden (map position steps)
  (loop with todo = (list position)
        with width = (input-map-width map)
        with height = (input-map-height map)
        for remaining-steps downfrom steps
        when (= remaining-steps 0)
          do (return (length todo))
        do (setf todo
                 (remove-duplicates
                  (loop for position in todo
                        nconc (neighbouring-gardens map width height position))
                  :test #'equal))))

(defun task-2 (map start-pos final-steps)
  (let* ((width (input-map-width map))
         (target (mod final-steps width))
         (f (floor final-steps width))
         (y0 (step-through-garden map start-pos target))
         (y1 (step-through-garden map start-pos (+ width target)))
         (y2 (step-through-garden map start-pos (+ (* width 2) target)))
         (b0 y0)
         (b1 (- y1 y0))
         (b2 (- y2 y1)))
    (+ (* (* f (/ (1- f) 2)) (- b2 b1)) (* b1 f) b0)))

(defun day-21 (input &optional (steps-1 64) (steps-2 26501365))
  (let* ((map (make-map input))
         (start-pos (find-start-position map)))
    (values (step-through-garden map start-pos steps-1)
            (task-2 map start-pos steps-2))))

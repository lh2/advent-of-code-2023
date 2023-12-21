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

(defun neighbouring-gardens (map width height position)
  (loop for nd in (let (nds)
                    (when (> (point-x position) 0)
                      (push (cons -1 0) nds))
                    (when (< (point-x position) (1- width))
                      (push (cons 1 0) nds))
                    (when (> (point-y position) 0)
                      (push (cons 0 -1) nds))
                    (when (< (point-y position) (1- height))
                      (push (cons 0 1) nds))
                    nds)
        for n = (point+ position nd)
        for cell = (map-cell map n)
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

(defun day-21 (input &optional (steps 64))
  (let* ((map (make-map input))
         (start-pos (find-start-position map)))
    (step-through-garden map start-pos steps)))

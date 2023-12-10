(defpackage #:aoc/day-10
  (:use #:cl #:aoc/utils)
  (:export #:day-10))
(in-package #:aoc/day-10)

;; screws up my syntax highlighting
(defconstant +char-pipe+ (code-char 124))

(defun walk (dir pipe)
  (switch (pipe)
    (+char-pipe+
     (switch (dir :test 'equal)
       ((cons 0 1) (cons 0 1))
       ((cons 0 -1) (cons 0 -1))))
    (#\-
     (switch (dir :test 'equal)
       ((cons 1 0) (cons 1 0))
       ((cons -1 0) (cons -1 0))))
    (#\L
     (switch (dir :test 'equal)
       ((cons 0 1) (cons 1 0))
       ((cons -1 0) (cons 0 -1))))
    (#\J
     (switch (dir :test 'equal)
       ((cons 0 1) (cons -1 0))
       ((cons 1 0) (cons 0 -1))))
    (#\7
     (switch (dir :test 'equal)
       ((cons 1 0) (cons 0 1))
       ((cons 0 -1) (cons -1 0))))
    (#\F
     (switch (dir :test 'equal)
       ((cons -1 0) (cons 0 1))
       ((cons 0 -1) (cons 1 0))))))

(defparameter *neighbouring-pipes-dirs* '((1 . 0)
                                          (0 . 1)
                                          (-1 . 0)
                                          (0 . -1)))

(defun find-start-pos (map)
  (loop for y from 0 below (input-map-height map)
        thereis (loop for x from 0 below (input-map-width map)
                      for point = (cons x y)
                      for pipe = (map-cell map point)
                      when (char= pipe #\S)
                        do (return point))))

(defun find-first-step (map start-pos)
  (loop for dir in *neighbouring-pipes-dirs*
        for new-point = (point+ start-pos dir)
        for new-dir = (walk dir (map-cell map new-point))
        when new-dir
          do (return (values dir new-point))))

(defun task-1 (map dir pos)
  (loop for steps from 1
        for pipe = (map-cell map pos)
        when (char= pipe #\S)
          do (return (/ steps 2))
        do (setf dir (walk dir pipe))
        do (assert dir)
        do (setf pos (point+ pos dir))))

(defun day-10 (input)
  (let* ((map (make-map input))
         (start-pos (find-start-pos map)))
    (multiple-value-bind (dir pos)
        (find-first-step map start-pos)
      (task-1 map dir pos))))

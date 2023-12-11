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

(defun find-position-pipe-type (pos started-at finished-at)
  (let ((diff-s (point- started-at pos))
        (diff-e (point- finished-at pos)))
    (or (switch (diff-s :test #'equal)
          ((cons 1 0)
           (switch (diff-e :test #'equal)
             ((cons 0 1) #\F)
             ((cons -1 0) #\-)
             ((cons 0 -1) #\L)))
          ((cons 0 1)
           (switch (diff-e :test #'equal)
             ((cons -1 0) #\7)
             ((cons 0 -1) +char-pipe+)))
          ((cons -1 0)
           (when (equal diff-e (cons 0 -1)) #\J)))
        (error "Invalid start and end nodes ~A / ~A" diff-s diff-e))))

(defun task-1 (map dir pos)
  (loop with loop = (make-hash-table :test 'equal)
        with coming-from = nil
        for steps from 1
        for pipe = (map-cell map pos)
        do (setf (gethash pos loop) pipe)
        when (char= pipe #\S)
          do (return (values (/ steps 2)
                             loop
                             coming-from))
        do (setf dir (walk dir pipe))
        do (assert dir)
        do (setf coming-from pos
                 pos (point+ pos dir))))

(defun crossing-pipe-p (pipe)
  (cond
    ((char= pipe +char-pipe+) t)
    ((char= pipe #\L) #\7)
    ((char= pipe #\F) #\J)))

(defun task-2 (map loop)
  (loop for y from 0 below (input-map-height map)
        for in-loop? = nil
        for crossing-when = nil
        sum (loop for x from 0 below (input-map-width map)
                  for point = (cons x y)
                  for pipe = (map-cell map point)
                  for loop-pipe = (gethash point loop)
                  for crossing-pipe? = (and loop-pipe
                                            (or (eql loop-pipe crossing-when)
                                                (crossing-pipe-p loop-pipe)))
                  when (characterp crossing-pipe?)
                    do (setf crossing-when crossing-pipe?
                             crossing-pipe? nil)
                  when crossing-pipe?
                    do (setf in-loop? (not in-loop?))
                  when (and in-loop? (not loop-pipe))
                    sum 1)))

(defun day-10 (input)
  (let* ((map (make-map input))
         (start-pos (find-start-pos map)))
    (multiple-value-bind (dir pos)
        (find-first-step map start-pos)
      (multiple-value-bind (task-1 loop coming-from)
          (task-1 map dir pos)
        (setf (gethash start-pos loop)
              (find-position-pipe-type start-pos pos coming-from))
        (values task-1 (task-2 map loop))))))

(defpackage #:aoc/day-18
  (:use #:cl #:aoc/utils)
  (:export #:day-18))
(in-package #:aoc/day-18)

(defun parse-line (line)
  (multiple-value-bind (dig-length)
      (parse-integer line :start 2 :junk-allowed t)
    (list (switch ((aref line 0))
            (#\U :up)
            (#\L :left)
            (#\R :right)
            (#\D :down))
          dig-length)))

(defun dir-diff (dir)
  (ecase dir
    (:up '(0 . -1))
    (:left '(-1 . 0))
    (:right '(1 . 0))
    (:down '(0 . 1))))

(defun fill-lagoon (map start-pos)
  (loop with todo = (list start-pos)
        while todo
        for current = (pop todo)
        do (setf (gethash current map) t)
        do (loop for n in '(:up :left :right :down)
                 for n-pos = (point+ current (dir-diff n))
                 unless (gethash n-pos map)
                   do (push n-pos todo))))

(defun find-start-pos (map min-x min-y)
  (loop for x from min-x
        for y from min-y
        for pos = (cons x y)
        when (gethash pos map)
          do (return (point+ pos (cons 1 1)))))

(defun lagoon-size (map)
  (multiple-value-bind (min-x min-y)
      (loop for key being the hash-keys of map
            for (x . y) = key
            minimize x into min-x
            minimize y into min-y
            maximize x into max-x
            maximize y into max-y
            finally (return (values min-x min-y max-x max-y)))
    (let ((start-pos (find-start-pos map min-x min-y)))
      (fill-lagoon map start-pos)
      (hash-table-count map))))

(defun day-18 (input)
  (loop with map = (let ((ht (make-hash-table :test 'equal)))
                     (setf (gethash (cons 0 0) ht) t)
                     ht)
        with current = (cons 0 0)
        for line = (read-line input nil)
        while line
        for (dir length) = (parse-line line)
        do (loop for i from 0 below length
                 do (setf current (point+ current (dir-diff dir)))
                 do (setf (gethash current map) t))
        finally (return (lagoon-size map))))

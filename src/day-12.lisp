(defpackage #:aoc/day-12
  (:use #:cl #:aoc/utils)
  (:export
   #:possible-arrangements
   #:day-12))
(in-package #:aoc/day-12)

(defun parse-line (line)
  (let* ((space-pos (position #\Space line))
         (springs (subseq line 0 space-pos))
         (groups (read-number-list line :start (1+ space-pos))))
    (values springs groups)))

(defun possible-arrangements (springs groups)
  (let ((count 0)
        (length (length springs)))
    (labels ((next (springs pos groups &optional (current-group-length 0))
               (when (>= pos length)
                 (when (if groups
                           (and (= (length groups) 1)
                                (= (car groups) current-group-length))
                           (= current-group-length 0))
                   (incf count))
                 (return-from next))
               (let ((current (aref springs pos))
                     (next-pos (1+ pos))
                     (not-in-group (= current-group-length 0))
                     (group-filled? (and groups
                                         (= (car groups) current-group-length))))
                 (when group-filled?
                   (pop groups))
                 (cond
                   ((char= current #\.)
                    (when (or not-in-group group-filled?)
                      (next springs next-pos groups 0)))
                   ((char= current #\#)
                    (unless group-filled?
                      (next springs next-pos groups (1+ current-group-length))))
                   ((char= current #\?)
                    (when (or not-in-group group-filled?)
                      (next springs next-pos groups 0))
                    (unless group-filled?
                      (next springs next-pos groups (1+ current-group-length))))))))
      (next springs 0 groups))
    count))

(defun day-12 (input)
  (loop for line = (read-line input nil)
        while line
        sum (multiple-value-bind (springs groups)
                (parse-line line)
              (possible-arrangements springs groups))))

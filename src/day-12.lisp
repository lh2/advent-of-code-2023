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
  (let ((cache (make-hash-table)))
    (labels ((next (springs groups current-group-length)
               (when (null springs)
                 (when (if groups
                           (and (= (length groups) 1)
                                (= (car groups) current-group-length))
                           (= current-group-length 0))
                   (return-from next 1))
                 (return-from next 0))
               (let ((current (car springs))
                     (not-in-group (= current-group-length 0))
                     (group-filled? (and groups
                                         (= (car groups) current-group-length)))
                     (count 0))
                 (when group-filled?
                   (pop groups))
                 (cond
                   ((char= current #\.)
                    (when (or not-in-group group-filled?)
                      (incf count (next-cache (cdr springs) groups 0))))
                   ((char= current #\#)
                    (unless group-filled?
                      (incf count (next-cache (cdr springs) groups (1+ current-group-length)))))
                   ((char= current #\?)
                    (when (or not-in-group group-filled?)
                      (incf count (next-cache (cdr springs) groups 0)))
                    (unless group-filled?
                      (incf count (next-cache (cdr springs) groups (1+ current-group-length))))))
                 count))
             (next-cache (springs groups current-group-length)
               (let ((key (logior (ash (length springs) 16)
                                  (ash (length groups) 8)
                                  current-group-length)))
                 (or (gethash key cache)
                     (setf (gethash key cache)
                           (next springs groups current-group-length))))))
      (next-cache springs groups 0))))

(defun day-12 (input)
  (loop with task-1 = 0
        with task-2 = 0
        for line = (read-line input nil)
        while line
        do (multiple-value-bind (springs groups)
               (parse-line line)
             (incf task-1 (possible-arrangements (coerce springs 'list) groups))
             (incf task-2 (possible-arrangements (coerce (string-join (loop repeat 5 collect springs) #\?)
                                                         'list)
                                                 (loop repeat 5 append groups))))
        finally (return (values task-1 task-2))))

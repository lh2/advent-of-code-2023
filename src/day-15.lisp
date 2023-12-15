(defpackage #:aoc/day-15
  (:use #:cl #:aoc/utils)
  (:export #:day-15))
(in-package #:aoc/day-15)

(defun next-element (input)
  (let ((next (loop for char = (read-char input nil)
                    while (and char (char/= char #\,))
                    when (char/= #\Newline)
                      collect char)))
    (when next
      (coerce next 'string))))

(defun calculate-hash (element)
  (loop with current = 0
        for char across element
        do (progn
             (incf current (char-code char))
             (setf current (* current 17))
             (setf current (rem current 256)))
        finally (return current)))

(defun parse-element (element)
  (loop for char across element
        for i from 0
        do (cond
             ((char= char #\-)
              (return (list (subseq element 0 i) :remove)))
             ((char= char #\=)
              (return (list (subseq element 0 i)
                            (parse-integer element :start (1+ i))))))))

(defun total-focusing-power (boxes)
  (loop for lenses across boxes
        for box from 1
        sum (loop for lens in lenses
                  for slot from 1
                  sum (* box slot (second lens)))))

(defun day-15 (input)
  (loop with boxes = (make-array 256 :initial-element nil)
        for element = (next-element input)
        while element
        for parsed-element = (parse-element element)
        for box = (calculate-hash (first parsed-element))
        do (setf (aref boxes box)
                 (if (eq (second parsed-element) :remove)
                     (remove (first parsed-element)
                             (aref boxes box)
                             :test #'equal
                             :key #'first)
                     (let* ((box-content (aref boxes box))
                            (existing (find (first parsed-element)
                                            box-content
                                            :test #'equal
                                            :key #'first)))
                       (if existing
                           (progn
                             (setf (second existing) (second parsed-element))
                             box-content)
                           (nconc box-content
                                  (list parsed-element))))))
        sum (claculate-hash element) into task-1
        finally (return (values task-1
                                (total-focusing-power boxes)))))

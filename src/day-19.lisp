(defpackage #:aoc/day-19
  (:use #:cl #:aoc/utils)
  (:export #:day-19))
(in-package #:aoc/day-19)

(defstruct part
  (x 0 :type fixnum)
  (m 0 :type fixnum)
  (a 0 :type fixnum)
  (s 0 :type fixnum))

(defun char-part-slot (char)
  (ecase char
    (#\x 'x)
    (#\m 'm)
    (#\a 'a)
    (#\s 's)))

(defun parse-condition (line start end)
  (let ((slot (char-part-slot (aref line start)))
        (op (aref line (1+ start))))
    (list (case op
            (#\< '<)
            (#\> '>))
          slot
          (parse-integer line
                         :start (+ start 2)
                         :end end))))

(defun parse-workflow (line)
  (let ((start-pos (position #\{ line)))
    (list (subseq line 0 start-pos)
          (loop for pos from (1+ start-pos) below (length line)
                for end = (or (position #\, line :start pos)
                              (position #\} line :start pos))
                for then-pos = (position #\: line :start pos :end end)
                if then-pos
                  collect (list (parse-condition line pos then-pos)
                                (subseq line (1+ then-pos) end))
                else
                  collect (list t (subseq line pos end))
                do (setf pos end)))))

(defun parse-workflows (input)
  (loop for line = (read-line input nil)
        while (and line (> (length line) 0))
        collect (parse-workflow line)))

(defun parse-part (line)
  (loop with part = (make-part)
        for pos from 1 below (length line)
        for field = (aref line pos)
        for (value end) = (multiple-value-list
                           (parse-integer line
                                          :start (+ pos 2)
                                          :junk-allowed t))
        do (setf pos end)
        do (setf (slot-value part (char-part-slot field))
                 value)
        finally (return part)))

(defun parse-parts (input)
  (loop for line = (read-line input nil)
        while line
        collect (parse-part line)))

(defun parse-input (input)
  (values (parse-workflows input)
          (parse-parts input)))

(defun compile-workflows (workflows)
  (let ((ht (make-hash-table))
        (syms (make-hash-table :test 'equal)))
    (labels ((make-workflow-symbol (name)
               (or (gethash name syms)
                   (setf (gethash name syms)
                         (make-symbol (string-upcase name)))))
             (call-workflow (sym part)
               (funcall (gethash sym ht) part)))
      (loop for (name conditions) in workflows
            for name-symbol = (make-workflow-symbol name)
            do (setf (gethash name-symbol ht)
                     (eval `(lambda (part)
                              (cond
                                ,@(loop for (condition then) in conditions
                                        unless (eq condition t)
                                          do (setf (second condition)
                                                   (ecase (second condition)
                                                     (x `(part-x part))
                                                     (m `(part-m part))
                                                     (a `(part-a part))
                                                     (s `(part-s part))))
                                        do (setf then (cond
                                                        ((string= then "A") t)
                                                        ((string= then "R") nil)
                                                        (t `(funcall ,#'call-workflow
                                                                     ',(make-workflow-symbol then)
                                                                     part))))
                                        collect (list condition then)))))))
      (gethash (make-workflow-symbol "in") ht))))

(defun day-19 (input)
  (multiple-value-bind (workflows parts)
      (parse-input input)
    (loop with in = (compile-workflows workflows)
          for part in parts
          when (funcall in part)
            sum (+ (part-x part)
                   (part-m part)
                   (part-a part)
                   (part-s part)))))

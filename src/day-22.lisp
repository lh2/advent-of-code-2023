(defpackage #:aoc/day-22
  (:use #:cl #:aoc/utils)
  (:export #:day-22))
(in-package #:aoc/day-22)

(defclass sand-block ()
  ((p-1
    :initarg :p-1
    :reader sand-block-p-1)
   (p-2
    :initarg :p-2
    :reader sand-block-p-2)))

(defun make-sand-block (p-1 p-2)
  (make-instance 'sand-block :p-1 p-1 :p-2 p-2))

(defmethod print-object ((block sand-block) stream)
  (print-unreadable-object (block stream :type t)
    (with-slots (p-1 p-2)
        block
      (format stream "~A ~A" p-1 p-2))))

(defmethod sand-block-bottom-z ((block sand-block))
  (with-slots (p-1)
      block
    (third p-1)))

(defmethod (setf sand-block-bottom-z) (new-z (block sand-block))
  (with-slots (p-1)
      block
    (setf (third p-1) new-z)))

(defmethod sand-block-top-z ((block sand-block))
  (with-slots (p-2)
      block
    (third p-2)))

(defmethod (setf sand-block-top-z) (new-z (block sand-block))
  (with-slots (p-2)
      block
    (setf (third p-2) new-z)))

(defun parse-line (line)
  (loop with p-1 = (list 0 0 0)
        with p-2 = (list 0 0 0)
        for current-index in (list 0 1 2
                                   0 1 2)
        for obj in (list p-1 p-1 p-1
                         p-2 p-2 p-2)
        for pos from 0 below (length line)
        do (multiple-value-bind (n end)
               (parse-integer line :start pos :junk-allowed t)
             (setf (nth current-index obj) n)
             (setf pos end))
        finally (return (make-sand-block p-1 p-2))))

(defun parse-input (input)
  (loop for line = (read-line input nil)
        while line
        collect (parse-line line)))

(defun collides-p (block-1 block-2)
  (labels ((intersects-p (from-1 to-1 from-2 to-2)
             (and (<= from-1 to-2)
                  (<= from-2 to-1))))
    (let ((b1-p1 (sand-block-p-1 block-1))
          (b1-p2 (sand-block-p-2 block-1))
          (b2-p1 (sand-block-p-1 block-2))
          (b2-p2 (sand-block-p-2 block-2)))
      (and (intersects-p (first b1-p1) (first b1-p2) (first b2-p1) (first b2-p2))
           (intersects-p (second b1-p1) (second b1-p2) (second b2-p1) (second b2-p2))))))

(defun find-collision (block others)
  (first (sort (loop for other in others
                     when (collides-p block other)
                       collect other)
               #'>
               :key #'sand-block-top-z)))

(defun apply-gravity (blocks)
  (loop for i from 0
        for block in blocks
        for others = (nreverse (subseq blocks 0 i))
        for collision = (find-collision block others)
        for new-z = (or (and collision
                             (1+ (sand-block-top-z collision)))
                        1)
        do (decf (sand-block-top-z block)
                 (- (sand-block-bottom-z block) new-z))
        do (setf (sand-block-bottom-z block) new-z)
        finally (return blocks)))

(defun task-1 (blocks)
  (loop with safe-to-disintegrate = (make-hash-table)
        for (block . others) on blocks
        for resting-z = (1- (sand-block-bottom-z block))
        for resting-on = (loop for other in others
                               when (and (= (sand-block-top-z other) resting-z)
                                         (collides-p block other))
                                 collect other)
        when (= (length resting-on) 1)
          do (setf (gethash (first resting-on) safe-to-disintegrate) t)
        finally (return (loop for block in blocks
                              when (not (gethash block safe-to-disintegrate))
                                sum 1))))

(defun day-22 (input)
  (let* ((blocks (parse-input input))
         (blocks (sort blocks #'< :key #'sand-block-bottom-z))
         (blocks (apply-gravity blocks)))
    (task-1 (nreverse blocks))))

(defpackage #:aoc/day-22
  (:use #:cl #:aoc/utils)
  (:export #:day-22))
(in-package #:aoc/day-22)

(defclass sand-block ()
  ((id
    :initarg :id)
   (p-1
    :initarg :p-1
    :reader sand-block-p-1)
   (p-2
    :initarg :p-2
    :reader sand-block-p-2)))

(defun make-sand-block (id p-1 p-2)
  (make-instance 'sand-block :id id :p-1 p-1 :p-2 p-2))

(defmethod print-object ((block sand-block) stream)
  (print-unreadable-object (block stream :type t)
    (with-slots (id p-1 p-2)
        block
      (format stream "~A: ~A ~A" id p-1 p-2))))

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

(defun parse-line (line index)
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
        finally (return (make-sand-block (code-char (+ index 65)) p-1 p-2))))

(defun parse-input (input)
  (loop for line = (read-line input nil)
        for i from 0
        while line
        collect (parse-line line i)))

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

(defun block-rests-on (block others)
  (loop with resting-z = (1- (sand-block-bottom-z block))
        for other in others
        when (and (= (sand-block-top-z other) resting-z)
                  (collides-p block other))
          collect other))

(defun blocks-resting-on (block others)
  (loop with resting-z = (1+ (sand-block-top-z block))
        for other in others
        when (and (= (sand-block-bottom-z other) resting-z)
                  (collides-p block other))
          collect other))

(defun task-1 (blocks)
  (loop with safe-to-disintegrate = (make-hash-table)
        for (block . others) on blocks
        for resting-on = (block-rests-on block others)
        when (= (length resting-on) 1)
          do (setf (gethash (first resting-on) safe-to-disintegrate) t)
        finally (return (loop for block in blocks
                              when (not (gethash block safe-to-disintegrate))
                                sum 1))))

(defun task-2 (blocks)
  (let ((c-block-rests-on (make-hash-table))
        (c-blocks-resting-on (make-hash-table)))
    (labels ((c-block-rests-on (block)
               (or (gethash block c-block-rests-on)
                   (setf (gethash block c-block-rests-on)
                         (block-rests-on block blocks))))
             (c-blocks-resting-on (block)
               (or (gethash block c-blocks-resting-on)
                   (setf (gethash block c-blocks-resting-on)
                         (blocks-resting-on block blocks))))
             (unsupported-p (block destroyed)
               (let* ((rests-on (c-block-rests-on block))
                      (rests-on (remove-if (lambda (block)
                                             (gethash block destroyed))
                                           rests-on)))
                 (null rests-on)))
             (destroy (layer &optional (destroyed (make-hash-table)))
               (loop for block in layer
                     do (setf (gethash block destroyed) t))
               (let ((next-layer (loop with result = (make-hash-table)
                                       for block in layer
                                       do (loop for resting-on in (c-blocks-resting-on block)
                                                when (unsupported-p resting-on destroyed)
                                                  do (setf (gethash resting-on result) t))
                                       finally (return (hash-table-keys result)))))
                 (+ (length layer)
                    (if next-layer (destroy next-layer destroyed) 0)))))
      (loop for block in blocks
            sum (max 0 (1- (destroy (list block))))))))

(defun day-22 (input)
  (let* ((blocks (parse-input input))
         (blocks (sort blocks #'< :key #'sand-block-bottom-z))
         (blocks (apply-gravity blocks)))
    (values (task-1 (reverse blocks))
            (task-2 blocks))))

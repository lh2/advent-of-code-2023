(defpackage #:aoc/day-17
  (:use #:cl #:aoc/utils)
  (:export #:day-17))
(in-package #:aoc/day-17)

(defparameter *dirs* '(:left (-1 . 0)
                       :up (0 . -1)
                       :right (1 . 0)
                       :down (0 . 1)))

(defparameter *next-dirs* '(:left (:up :down)
                            :up (:left :right)
                            :right (:up :down)
                            :down (:left :right)))

(defun make-side-in-map-p (map closed)
  (lambda (side)
    (or (< (point-x (car side)) 0)
        (< (point-y (car side)) 0)
        (>= (point-x (car side)) (input-map-width map))
        (>= (point-y (car side)) (input-map-height map))
        (gethash side closed))))

(defun next-steps-crucible (map pos dir steps closed)
  (let (sides)
    (when (< steps 3)
      (push (list (point+ pos (getf *dirs* dir))
                  dir
                  (1+ steps))
            sides))
    (loop for next in (getf *next-dirs* dir)
          do (push (list (point+ pos (getf *dirs* next))
                         next
                         1)
                   sides))
    (remove-if (make-side-in-map-p map closed) sides)))

(defun next-steps-ultra-crucible (map pos dir steps closed)
  (let (sides)
    (when (< steps 10)
      (push (list (point+ pos (getf *dirs* dir))
                  dir
                  (1+ steps))
            sides))
    (when (> steps 3)
      (loop for next in (getf *next-dirs* dir)
            do (push (list (point+ pos (getf *dirs* next))
                           next
                           1)
                     sides)))
    (remove-if (make-side-in-map-p map closed) sides)))

(defun find-path-with-lowest-head-loss (map next-steps)
  (loop with todo = (let ((q (make-queue :priority-queue :compare (lambda (a b)
                                                                    (< (fourth a)
                                                                       (fourth b))))))
                      (qpush q (list (cons 0 0) :right 0 0 nil))
                      (qpush q (list (cons 0 0) :down 0 0 nil))
                      q)
        with closed = (make-hash-table :test 'equal)
        with goal = (cons (1- (input-map-width map))
                          (1- (input-map-height map)))
        while (> (qsize todo) 0)
        for (pos dir steps heat-loss) = (qpop todo)
        when (equal pos goal)
          do (return heat-loss)
        do (setf (gethash (list pos dir steps) closed) t)
        do (loop for (next next-dir steps) in (funcall next-steps map pos dir steps closed)
                 for cost = (char-number (map-cell map next))
                 for next-heat-cost = (+ heat-loss cost)
                 for existing = (queue-find todo (lambda (existing)
                                                   (and (equal (first existing) next)
                                                        (eq (second existing) next-dir)
                                                        (eql (third existing) steps))))
                 for existing-value = (and existing
                                           (queues::node-value existing))
                 do (cond
                      ((and existing-value
                            (< next-heat-cost (fourth existing-value)))
                       (setf (fourth existing-value) next-heat-cost))
                      ((null existing-value)
                       (qpush todo (list next next-dir steps next-heat-cost)))))))

(defun day-17 (input)
  (let ((map (make-map input)))
    (values (find-path-with-lowest-head-loss map #'next-steps-crucible)
            (find-path-with-lowest-head-loss map #'next-steps-ultra-crucible))))

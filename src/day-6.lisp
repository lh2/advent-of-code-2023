(defpackage #:aoc/day-6
  (:use #:cl #:aoc/utils)
  (:export #:day-6))
(in-package #:aoc/day-6)

(declaim (ftype (function (fixnum fixnum fixnum) t) beats-distance-p)
         (inline beats-distance-p))
(defun beats-distance-p (speed time distance)
  (let* ((remaining-time (- time speed))
         (travelled (the fixnum (* speed remaining-time))))
    (> travelled distance)))

(declaim (ftype (function (fixnum fixnum) fixnum) possible-ways-to-beat))
(defun possible-ways-to-beat (time distance)
  (let ((start (loop for speed from 1 below time
                     when (beats-distance-p speed time distance)
                       do (return speed))))
    (- time (* start 2) -1)))

(defun join-numbers (numbers)
  (parse-integer (apply 'concatenate 'string
                        (mapcar #'write-to-string numbers))))

(defun day-6 (input)
  (let* ((line (read-line input))
         (times (read-number-list line :start (1+ (position #\: line))))
         (line (read-line input))
         (distances (read-number-list line :start (1+ (position #\: line)))))
    (values (loop with task-1 fixnum = 1
                  for time in times
                  for distance in distances
                  do (setf task-1 (* task-1 (possible-ways-to-beat time distance)))
                  finally (return task-1))
            (possible-ways-to-beat (join-numbers times)
                                   (join-numbers distances)))))

(defpackage #:aoc/day-6
  (:use #:cl #:aoc/utils)
  (:export #:day-6))
(in-package #:aoc/day-6)

(defun task-1 (times distances)
  (loop with task-1 = nil
        for time in times
        for distance in distances
        do (push (loop for speed from 1 below time
                       for remaining-time = (- time speed)
                       for travelled = (* speed remaining-time)
                       when (> travelled distance)
                         sum 1 fixnum)
                 task-1)
        finally (return (apply #'* task-1))))

(defun join-numbers (numbers)
  (parse-integer (apply 'concatenate 'string
                        (mapcar #'write-to-string numbers))))

(defun task-2 (time distance)
  (let ((start (loop for speed from 1 below time
                     for remaining-time = (- time speed)
                     for travelled = (* speed remaining-time)
                     when (> travelled distance)
                       do (return speed)))
        (end (loop for speed from (1- time) downto 1
                   for remaining-time = (- time speed)
                   for travelled = (* speed remaining-time)
                   when (> travelled distance)
                     do (return speed))))
    (- end start -1)))

(defun day-6 (input)
  (let* ((line (read-line input))
         (times (read-number-list line :start (1+ (position #\: line))))
         (line (read-line input))
         (distances (read-number-list line :start (1+ (position #\: line)))))
    (values (task-1 times distances)
            (task-2 (join-numbers times)
                    (join-numbers distances)))))

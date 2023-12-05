(defpackage #:aoc/day-5
  (:use #:cl #:aoc/utils)
  (:export #:day-5))
(in-package #:aoc/day-5)

(defun read-number-list (string &key (start 0))
  (loop for i from start below (length string)
        collect (multiple-value-bind (number end)
                    (parse-integer string
                                   :start i
                                   :junk-allowed t)
                  (setf i end)
                  number)))

(defun parse-seeds (input)
  (let ((line (prog1
                  (read-line input)
                (read-line input))))
    (read-number-list line :start (1+ (position #\: line)))))

(defun parse-maps (input)
  (loop for header = (read-line input nil)
        while header
        collect (loop for line = (read-line input nil)
                      while (and line (string/= line ""))
                      collect (read-number-list line))))

(defun map-number (number map)
  (or (loop for (dest source length) in map
            for diff = (- number source)
            when (and (>= diff 0)
                      (< diff length))
              do (return (+ dest diff)))
      number))

(defun day-5 (input)
  (loop with seeds = (parse-seeds input)
        with maps = (parse-maps input)
        for seed in seeds
        minimize (loop with n = seed
                       for map in maps
                       do (setf n (map-number n map))
                       finally (return n))))

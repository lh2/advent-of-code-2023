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

(declaim (ftype (function (fixnum list) fixnum) map-number))
(defun map-number (number map)
  (or (loop for (dest source length) (fixnum fixnum fixnum) in map
            for diff fixnum = (- number source)
            when (and (>= diff 0)
                      (< diff length))
              do (return (the fixnum (+ dest diff))))
      number))

(defun map-seed (seed maps)
  (loop with n = seed
        for map in maps
        do (setf n (map-number n map))
        finally (return n)))

(defun day-5 (input)
  (let ((seeds (parse-seeds input))
        (maps (parse-maps input)))
    (values
     (loop for seed in seeds
           minimize (map-seed seed maps) fixnum)
     (loop for (seed-start seed-length) on seeds by #'cddr
           minimize (loop for seed from seed-start
                          repeat seed-length
                          minimize (map-seed seed maps) fixnum) fixnum
           do (format t "Seed range ~A ~A done!~%" seed-start seed-length)))))

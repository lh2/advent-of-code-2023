(defpackage #:aoc/day-5
  (:use #:cl #:aoc/utils)
  (:export #:day-5))
(in-package #:aoc/day-5)

(defun parse-seeds (input)
  (let ((line (prog1
                  (read-line input)
                (read-line input))))
    (read-number-list line :start (1+ (position #\: line)))))

(defun parse-maps (input)
  (loop for header = (read-line input nil)
        while header
        collect (sort (loop for line = (read-line input nil)
                            while (and line (string/= line ""))
                            collect (read-number-list line))
                      (lambda (a b)
                        (< (second a) (second b))))))

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

(defun split-range (range map)
  (declare (optimize speed))
  (destructuring-bind (start range-length)
      range
    (declare (type fixnum start range-length))
    (loop with range-end fixnum = (+ start range-length -1)
          for (dest source length) (fixnum fixnum fixnum) in map
          while (<= source range-end)
          for end-map fixnum = (+ source length -1)
          when (< start source)
            collect (list start (the fixnum (- source start))) into split-ranges
            and do (setf start source)
          do (decf length (+ (- start source)))
          do (decf length (max (- (the fixnum (+ start length -1))
                                  range-end)
                               0))
          when (>= end-map start)
            collect (list (the fixnum (+ dest (- start source))) length) into split-ranges
            and do (incf start length)
          finally (return (if (< start range-end)
                              (cons (list start (the fixnum (- range-end start -1)))
                                    split-ranges)
                              split-ranges)))))

(defun split-ranges (ranges map)
  (loop for range in ranges
        nconc (split-range range map)))

(defun task-2 (ranges maps)
  (dolist (map maps)
    (setf ranges (split-ranges ranges map)))
  (loop for range in ranges
        minimize (car range) fixnum))

(defun day-5 (input)
  (let* ((seeds (parse-seeds input))
         (maps (parse-maps input)))
    (values
     (loop for seed in seeds
           minimize (map-seed seed maps) fixnum)
     (task-2 (loop for (seed-start seed-length) on seeds by #'cddr
                   collect (list seed-start seed-length))
             maps))))

(defpackage #:aoc/day-18
  (:use #:cl #:aoc/utils)
  (:export #:day-18))
(in-package #:aoc/day-18)

(defun parse-line (line)
  (multiple-value-bind (dig-length end)
      (parse-integer line :start 2 :junk-allowed t)
    (list (switch ((aref line 0))
            (#\U :up)
            (#\L :left)
            (#\R :right)
            (#\D :down))
          dig-length
          (switch ((aref line (+ end 3 5)))
            (#\0 :right)
            (#\1 :down)
            (#\2 :left)
            (#\3 :up))
          (parse-integer line
                         :start (+ end 3)
                         :end (+ end 3 5)
                         :radix 16))))

(defun dir-diff (dir length)
  (ecase dir
    (:up (cons 0 (- length)))
    (:left (cons (- length) 0))
    (:right (cons length 0))
    (:down (cons 0 length))))

(declaim (ftype (function ((simple-array cons)) fixnum) shoelace))
(defun shoelace (vertices)
  (loop with n = (- (length vertices) 2)
        repeat n
        for i from 0
        for j = (1+ i)
        for x-1 fixnum = (car (aref vertices i))
        for y-1 fixnum = (cdr (aref vertices i))
        for x-2 fixnum = (car (aref vertices j))
        for y-2 fixnum = (cdr (aref vertices j))
        sum (the fixnum (* x-1 y-2)) into sum-1 fixnum
        sum (the fixnum (* x-2 y-1)) into sum-2 fixnum
        finally (return (/ (the fixnum (abs (- sum-1 sum-2))) 2))))


(defun day-18 (input)
  (loop with vertices-1 = (list (cons 0 0))
        with vertices-2 = (list (cons 0 0))
        with current-1 = (cons 0 0)
        with current-2 = (cons 0 0)
        for line = (read-line input nil)
        while line
        for (dir-1 length-1 dir-2 length-2) = (parse-line line)
        sum length-1 into length-1-sum
        sum length-2 into length-2-sum
        do (setf current-1 (point+ current-1 (dir-diff dir-1 length-1)))
        do (setf current-2 (point+ current-2 (dir-diff dir-2 length-2)))
        do (push current-1 vertices-1)
        do (push current-2 vertices-2)
        finally (return (values (+ (shoelace (coerce (nreverse vertices-1) 'vector))
                                   (/ length-1-sum 2)
                                   1)
                                (+ (shoelace (coerce (nreverse vertices-2) 'vector))
                                   (/ length-2-sum 2)
                                   1)))))

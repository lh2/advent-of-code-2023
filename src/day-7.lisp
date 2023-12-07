(defpackage #:aoc/day-7
  (:use #:cl #:aoc/utils)
  (:export #:day-7))
(in-package #:aoc/day-7)

(defstruct hand-data
  (hand "" :type simple-string)
  (bid 0 :type fixnum)
  (hand-strength -1 :type fixnum)
  (cards-strength -1 :type fixnum))

(declaim (ftype (function (simple-string) hand-data) parse-hand-data))
(defun parse-hand-data (line)
  (let ((pos (position #\Space line)))
    (make-hand-data :hand (subseq line 0 pos)
                    :bid (parse-integer line :start (1+ pos)))))

(defun parse-input (input)
  (loop for line = (read-line input nil)
        while line
        collect (parse-hand-data line)))

(declaim (type (simple-array fixnum) *cards*))
(defparameter *cards*
  (make-array
   35
   :element-type 'fixnum
   :initial-contents (loop for card from 50 to 84
                           collect (or (case (code-char card)
                                         (#\A 13) (#\K 12) (#\Q 11) (#\J 10) (#\T 9)
                                         (#\9 8) (#\8 7) (#\7 6) (#\6 5) (#\5 4)
                                         (#\4 3) (#\3 2) (#\2 1))
                                       0))))

(defparameter *cards-jokering*
  (let ((copy (copy-seq *cards*)))
    (setf (aref copy (- (char-code #\J) 50)) 0)
    copy))

(declaim (inline card-strength)
         (ftype (function (character) fixnum) card-strength))
(defun card-strength (card)
  (aref *cards* (- (char-code card) 50)))

(defparameter *with-jokering* nil)

(defun hand-strength (hand)
  (let* ((groups (group-by (coerce (if *with-jokering*
                                       (remove #\J hand)
                                       hand)
                                   'list)
                           :key #'identity
                           :value #'identity
                           :test #'char=))
         (groups (mapcar (lambda (group)
                           (declare (type list group))
                           (1- (length group)))
                         groups))
         (groups (sort groups #'>)))
    (when *with-jokering*
      (let ((n-jokers (count #\J hand)))
        (declare (type fixnum n-jokers))
        (when (= n-jokers 5)
          (return-from hand-strength 70))
        (setf groups
              (loop for group fixnum in groups
                    for missing fixnum = (- 5 group)
                    for usable-jokers = (min missing n-jokers)
                    when (> missing 0)
                      do (incf group usable-jokers)
                      and do (decf n-jokers usable-jokers)
                    collect group))))
    (let* ((n-pairs (count 2 groups))
           (highest-sequence (first groups)))
      (declare (type fixnum n-pairs highest-sequence))
      (cond
        ((= highest-sequence 5) 70)
        ((= highest-sequence 4) 60)
        ((and (= highest-sequence 3)
              (= n-pairs 1))
         50)
        ((= highest-sequence 3) 40)
        ((= n-pairs 2) 30)
        ((= n-pairs 1) 20)
        (t (card-strength (first-elt hand)))))))

(declaim (ftype (function (simple-string) fixnum) card-strengths))
(defun card-strengths (hand)
  (loop with sum-card-strengths fixnum = 0
        for card across hand
        do (setf sum-card-strengths
                 (+ (the fixnum (* sum-card-strengths 20))
                    (card-strength card)))
        finally (return sum-card-strengths)))

(defun calculate-strengths (hands-data)
  (loop for hand-data of-type hand-data in hands-data
        do (setf (hand-data-hand-strength hand-data)
                 (hand-strength (hand-data-hand hand-data))

                 (hand-data-cards-strength hand-data)
                 (card-strengths (hand-data-hand hand-data)))))

(defun hand-data< (hand-data-1 hand-data-2)
  (let ((strength-1 (hand-data-hand-strength hand-data-1))
        (strength-2 (hand-data-hand-strength hand-data-2)))
    (if (= strength-1 strength-2)
        (progn
          (< (hand-data-cards-strength hand-data-1)
             (hand-data-cards-strength hand-data-2)))
        (< strength-1 strength-2))))

(declaim (ftype (function (list) fixnum) total-winnings))
(defun total-winnings (hands-data)
  (calculate-strengths hands-data)
  (let ((hands-data (sort hands-data #'hand-data<)))
    (loop for hand-data of-type hand-data in hands-data
          for rank fixnum from 1
          sum (the fixnum (* (hand-data-bid hand-data) rank)) fixnum)))

(defmacro with-jokering (() &body body)
  `(let ((*cards* *cards-jokering*)
         (*with-jokering* t))
     ,@body))

(defun day-7 (input)
  (let* ((hands-data-task-1 (parse-input input))
         (hands-data-task-2 (copy-seq hands-data-task-1)))
    (values (total-winnings hands-data-task-1)
            (with-jokering ()
              (total-winnings hands-data-task-2)))))

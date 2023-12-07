(defpackage #:aoc/day-7
  (:use #:cl #:aoc/utils)
  (:export #:day-7))
(in-package #:aoc/day-7)

(defun parse-hand-bid (line)
  (let ((pos (position #\Space line)))
    (list (coerce (subseq line 0 pos) 'list)
          (parse-integer line :start (1+ pos)))))

(defun parse-input (input)
  (loop for line = (read-line input nil)
        while line
        collect (parse-hand-bid line)))

(defparameter *cards* (nconc (list #\A #\K #\Q #\J #\T)
                             (loop for i from 9 downto 2 collect (code-char (+ i 48)))))

(defun card-strength (card)
  (abs (- (position card *cards*) 13)))

(defun make-pair-detector (length)
  (lambda (group)
    (= (length (cdr group)) length)))

(defparameter *pair-detector-five* (make-pair-detector 5))
(defparameter *pair-detector-four* (make-pair-detector 4))
(defparameter *pair-detector-three* (make-pair-detector 3))
(defparameter *pair-detector-two* (make-pair-detector 2))

(defun hand-strength (hand)
  (let* ((groups (group-by hand
                           :key #'identity
                           :value #'identity
                           :test #'char=))
         (three (find-if *pair-detector-three* groups))
         (n-pairs (length (remove-if-not *pair-detector-two* groups))))
    (cond
      ((some *pair-detector-five* groups) 70)
      ((some *pair-detector-four* groups) 60)
      ((and three
            (= n-pairs 1))
       50)
      (three 40)
      ((= n-pairs 2) 30)
      ((= n-pairs 1) 20)
      (t (card-strength (first hand))))))

(defun calculate-strengths (hands)
  (loop for (hand bid) in hands
        collect (list (hand-strength hand)
                      hand
                      bid)))

(defun hand-strength< (hand-1 hand-2)
  (let* ((strength-1 (car hand-1))
         (strength-2 (car hand-2)))
    (if (= strength-1 strength-2)
        (loop for card-1 in (second hand-1)
              for card-2 in (second hand-2)
              while (char= card-1 card-2)
              finally (return (< (card-strength card-1)
                                 (card-strength card-2))))
        (< strength-1 strength-2))))

(defun day-7 (input)
  (let* ((hands-bids (parse-input input))
         (strength-hands-bids (calculate-strengths hands-bids))
         (strength-hands-bids (sort strength-hands-bids #'hand-strength<)))
    (loop for (hand-strength hand bid) in strength-hands-bids
          for rank from 1
          sum (* bid rank))))

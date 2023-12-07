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

(defparameter *cards-jokering* (nconc (list #\A #\K #\Q #\T)
                                      (loop for i from 9 downto 2 collect (code-char (+ i 48)))
                                      (list #\J)))

(defun card-strength (card)
  (abs (- (position card *cards*) 13)))

(defparameter *with-jokering* nil)

(defun hand-strength (hand)
  (let* ((groups (group-by (if *with-jokering*
                               (remove #\J hand)
                               hand)
                           :key #'identity
                           :value #'identity
                           :test #'char=))
         (groups (mapcar (lambda (group)
                           (1- (length group)))
                         groups))
         (groups (sort groups #'>))
         (n-jokers (when *with-jokering*
                     (count #\J hand))))
    (when *with-jokering*
      (when (= n-jokers 5)
        (return-from hand-strength 70))
      (setf groups
            (loop for group in groups
                  for missing = (- 5 group)
                  for usable-jokers = (min missing n-jokers)
                  when (> missing 0)
                    do (incf group usable-jokers)
                    and do (decf n-jokers usable-jokers)
                  collect group)))
    (let* ((n-pairs (count 2 groups))
           (highest-sequence (first groups)))
      (cond
        ((= highest-sequence 5) 70)
        ((= highest-sequence 4) 60)
        ((and (= highest-sequence 3)
              (= n-pairs 1))
         50)
        ((= highest-sequence 3) 40)
        ((= n-pairs 2) 30)
        ((= n-pairs 1) 20)
        (t (card-strength (first hand)))))))

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

(defun total-winnings (hands-bids)
  (let* ((strength-hands-bids (calculate-strengths hands-bids))
         (strength-hands-bids (sort strength-hands-bids #'hand-strength<)))
    (loop for (hand-strength hand bid) in strength-hands-bids
          for rank from 1
          sum (* bid rank))))

(defmacro with-jokering (() &body body)
  `(let ((*cards* *cards-jokering*)
         (*with-jokering* t))
     ,@body))

(defun day-7 (input)
  (let ((hands-bids (parse-input input)))
    (values (total-winnings hands-bids)
            (with-jokering ()
              (total-winnings hands-bids)))))

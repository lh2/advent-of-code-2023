(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defun schematic-symbol-p (symbol)
  (and (char/= symbol #\.)
       (not (digit-char-p symbol))))

(declaim (inline digit-at-p))
(defun digit-at-p (map y x)
  (digit-char-p (aref map y x)))

(defun extract-part-number (map y start-x)
  (loop for x from start-x below (array-dimension map 1)
        for char = (aref map y x)
        while (digit-char-p char)
        collect char into chars
        finally (return (values (parse-integer (coerce chars 'string))
                                (1- x)))))

(defun find-part-number (map y x)
  (extract-part-number map
                       y
                       (loop for px from x downto 0
                             while (digit-at-p map y px)
                             finally (return (1+ px)))))

(defun gear-neighbouring-part-numbers (map y x)
  (let ((left-open? (> x 0))
        (right-open? (< x (array-dimension map 1)))
        (top-open? (> y 0))
        (bottom-open? (< y (array-dimension map 0)))
        (numbers))
    (when (and left-open?
               (digit-at-p map y (1- x)))
      (push (find-part-number map y (1- x)) numbers)      )
    (when (and right-open?
               (digit-at-p map y (1+ x)))
      (push (find-part-number map y (1+ x)) numbers))
    (when top-open?
      (if (digit-at-p map (1- y) x)
          (push (find-part-number map (1- y) x) numbers)
          (progn
            (when (and left-open?
                       (digit-at-p map (1- y) (1- x)))
              (push (find-part-number map (1- y) (1- x)) numbers))
            (when (and right-open?
                       (digit-at-p map (1- y) (1+ x)))
              (push (find-part-number map (1- y) (1+ x)) numbers)))))
    (when bottom-open?
      (if (digit-at-p map (1+ y) x)
          (push (find-part-number map (1+ y) x) numbers)
          (progn
            (when (and left-open?
                       (digit-at-p map (1+ y) (1- x)))
              (push (find-part-number map (1+ y) (1- x)) numbers))
            (when (and right-open?
                       (digit-at-p map (1+ y) (1+ x)))
              (push (find-part-number map (1+ y) (1+ x)) numbers)))))
    numbers))

(defun calculate-gear-ratio (map y x)
  (let ((neighbouring-part-numbers (gear-neighbouring-part-numbers map y x)))
    (if (= (length neighbouring-part-numbers) 2)
        (apply #'* neighbouring-part-numbers)
        0)))

(defun day-3 (input)
  (loop with map = (make-map input)
        with task-1 = 0
        with task-2 = 0
        for y from 0 below (array-dimension map 0)
        do (loop with number-start = nil
                 for x from 0 below (array-dimension map 1)
                 for char = (aref map y x)
                 for digit? = (digit-char-p char)
                 when (and digit? (null number-start))
                   do (setf number-start x)
                 unless digit?
                   do (setf number-start nil)
                 when (and digit?
                           (member-if #'schematic-symbol-p
                                      (map-neighbours map y x)))
                   do (multiple-value-bind (number next)
                          (extract-part-number map y number-start)
                        (setf x next)
                        (incf task-1 number))
                 when (char= char #\*)
                   do (incf task-2 (calculate-gear-ratio map y x)))
        finally (return (values task-1 task-2))))

(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defun schematic-symbol-p (symbol)
  (and (char/= symbol #\.)
       (not (digit-char-p symbol))))

(declaim (inline digit-at-p))
(defun digit-at-p (map point)
  (digit-char-p (map-cell map point)))

(defun number-borders-symbol (map y x-1 x-2 top-open? bottom-open? left-open? width)
  (labels ((scan-line (y)
             (loop for x from x-1 to x-2
                   for cell = (map-cell map (cons x y))
                   thereis (schematic-symbol-p cell))))
    (or (when left-open?
          (decf x-1)
          (schematic-symbol-p (map-cell map (cons x-1 y))))
        (when (< x-2 (1- width))
          (incf x-2)
          (schematic-symbol-p (map-cell map (cons x-2 y))))
        (and top-open?
             (scan-line (1- y)))
        (and bottom-open?
             (scan-line (1+ y))))))

(declaim (ftype (function (input-map cons) fixnum) read-part-number))
(defun read-part-number (map point)
  (destructuring-bind (x . y)
      point
    (map-integer-at map (cons (loop for sx from x downto 0
                                    while (digit-at-p map (cons sx y))
                                    finally (return (1+ sx)))
                              y))))

(defun get-gear-ratio (map point top-open? bottom-open? left-open? right-open?)
  (macrolet ((push-neighbor-if-digit (check neighbour)
               `(let ((p (point+ point ,neighbour)))
                  (when (and ,check
                             (digit-at-p map p))
                    (push p number-points)))))
    (let ((number-points))
      (push-neighbor-if-digit left-open? '(-1 . 0))
      (push-neighbor-if-digit right-open? '(1 . 0))
      (when top-open?
        (unless (push-neighbor-if-digit t '(0 . -1))
          (push-neighbor-if-digit left-open? '(-1 . -1))
          (push-neighbor-if-digit right-open? '(1 . -1))))
      (when bottom-open?
        (unless (push-neighbor-if-digit t '(0 . 1))
          (push-neighbor-if-digit left-open? '(-1 . 1))
          (push-neighbor-if-digit right-open? '(1 . 1))))
      (when (/= (length number-points) 2)
        (return-from get-gear-ratio 0))
      (* (read-part-number map (first number-points))
         (read-part-number map (second number-points))))))

(defun day-3 (input)
  (loop with map = (make-map input)
        with task-1 fixnum = 0
        with task-2 fixnum = 0
        with width = (input-map-width map)
        with height = (input-map-height map)
        for y from 0 below height
        for top-open? = (> y 0)
        for bottom-open? = (< y (1- height))
        do (loop for x from 0 below (input-map-width map)
                 for left-open? = (> x 0)
                 for right-open? = (< x (1- width))
                 for point = (cons x y)
                 for cell = (map-cell map point)
                 do (cond
                      ((digit-char-p cell)
                       (multiple-value-bind (number end)
                           (map-integer-at map point)
                         (decf end)
                         (when (number-borders-symbol map y x end
                                                      top-open? bottom-open?
                                                      left-open? width)
                           (incf task-1 number))
                         (setf x end)))
                      ((char= cell #\*)
                       (incf task-2 (get-gear-ratio map point
                                                    top-open? bottom-open?
                                                    left-open? right-open?)))))
        finally (return (values task-1 task-2))))

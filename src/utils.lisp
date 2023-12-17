(uiop:define-package #:aoc/utils
  (:use #:cl)
  (:mix-reexport #:alexandria #:serapeum #:split-sequence
                 #:group-by #:str #:queues)
  (:import-from #:queues.simple-queue)
  (:import-from #:queues.priority-queue)
  (:export
   #:read-input
   #:read-input-fields
   #:read-input-match
   #:char-number
   #:make-map
   #:print-map
   #:input-map
   #:input-map-width
   #:input-map-height
   #:map-cell
   #:map-integer-at
   #:point+
   #:point-
   #:point-x
   #:point-y
   #:point-neighbours
   #:manhattan-distance
   #:do-map-neighbours
   #:read-number-list
   #:find-pattern))
(in-package #:aoc/utils)

(defun normalize-type (type)
  (cond
    ((or (eq type 'string)
         (null type))
     'simple-string)
    ((eq type 'number)
     'integer)
    (t type)))

(defun wrap-nullable (converter)
  (lambda (line)
    (if (= (length line) 0)
        nil
        (funcall converter line))))

(defun get-type-converter (type)
  (wrap-nullable
   (if (functionp type)
       type
       (ecase (normalize-type type)
         (simple-string #'identity)
         (integer #'parse-integer)
         (keyword (compose #'make-keyword #'string-upcase))))))

(defun read-input (input &key (type 'string))
  (loop with converter = (get-type-converter type)
        for line = (read-line input nil)
        while line
        collect (funcall converter line)))

(defun convert-fields (converters fields)
  (loop for converter in converters
        for field = (pop fields)
        collect (funcall converter field)))

(defun read-input-fields (input field-types &key (delimiter " "))
  (loop with converters = (mapcar #'get-type-converter
                                  field-types)
        for line = (read-line input nil)
        while line
        collect (convert-fields converters
                                (split-sequence delimiter line :test #'string=))))

(defun read-input-match (input regex &key types)
  (loop with scanner = (ppcre:create-scanner regex)
        with converters = (and types (mapcar #'get-type-converter types))
        for line = (read-line input nil)
        for groups = (and line
                          (multiple-value-bind (match groups)
                              (ppcre:scan-to-strings scanner line)
                            (and match (coerce groups 'list))))
        while groups
        collect (if converters
                    (convert-fields converters groups)
                    groups)))


(declaim (ftype (function (character) fixnum) char-number)
         (inline char-number))
(defun char-number (char)
  (- (char-int char) 48))

(defstruct input-map
  (data nil :type (simple-array simple-string))
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defun make-map (input)
  (loop with width = nil
        with data = nil
        for row = (read-line input nil)
        for height from 0
        while (and row (> (length row) 0))
        when (= height 0)
          do (setf width (length row))
        do (push row data)
        finally (return (and data
                             (make-input-map :data (coerce (nreverse data) 'vector)
                                             :width width
                                             :height height)))))

(defun print-map (map &key (stream *standard-output*))
  (loop for y from 0 below (input-map-height map)
        do (format stream "~A~%" (aref (input-map-data map) y))))

(declaim (inline point+ point- point-x point-y)
         (ftype (function (cons) fixnum) point-x point-y))

(defun point-x (point)
  (car point))

(defun point-y (point)
  (cdr point))

(defun point+ (point-a point-b)
  (cons (the fixnum (+ (point-x point-a)
                       (point-x point-b)))
        (the fixnum (+ (point-y point-a)
                       (point-y point-b)))))

(defun point- (point-a point-b)
  (cons (the fixnum (- (point-x point-a)
                       (point-x point-b)))
        (the fixnum (- (point-y point-a)
                       (point-y point-b)))))

(declaim (inline map-cell map-integer-at (setf map-cell))
         (ftype (function (input-map cons) character) map-cell)
         (ftype (function (character input-map cons) character) (setf map-cell)))

(defun map-cell (map point)
  (aref (aref (input-map-data map)
              (point-y point))
        (point-x point)))

(defun (setf map-cell) (new map point)
  (let ((row (aref (input-map-data map)
                   (point-y point))))
    (setf (aref row (point-x point)) new)))

(defun map-integer-at (map point)
  (parse-integer (aref (input-map-data map) (point-y point))
                 :start (point-x point)
                 :junk-allowed t))

(defparameter *map-neighbours* (loop for y from -1 to 1
                                     nconc (loop for x from -1 to 1
                                                 when (not (and (= y 0)
                                                                (= x 0)))
                                                   collect (cons x y))))

(defun point-neighbours (point)
  (mapcar (curry #'point+ point)
          *map-neighbours*))

(defun manhattan-distance (from to)
  (+ (abs (- (point-x to)
             (point-x from)))
     (abs (- (point-y to)
             (point-y from)))))

(defmacro do-map-neighbours ((neighbour-point map start-point) &body body)
  (with-gensyms (width height lb? rb? tb? bb?)
    (once-only ((sp start-point)
                (mp map))
      `(let* ((,width (input-map-width ,mp))
              (,height (input-map-height ,mp))
              (,lb? (> (point-x ,sp) 0))
              (,rb? (< (point-x ,sp) (1- ,width)))
              (,tb? (> (point-y ,sp) 0))
              (,bb? (< (point-y ,sp) (1- ,height))))
         ,@(loop for nb in *map-neighbours*
                 collect `(let ((,neighbour-point (point+ ,sp ',nb)))
                            (when (and ,@(let ((checks))
                                           (when (< (point-x nb) 0)
                                             (push lb? checks))
                                           (when (< (point-y nb) 0)
                                             (push tb? checks))
                                           (when (> (point-x nb) 0)
                                             (push rb? checks))
                                           (when (> (point-y nb) 0)
                                             (push bb? checks))
                                           checks))
                              ,@body)))))))

(defun read-number-list (string &key (start 0))
  (loop for i from start below (length string)
        collect (multiple-value-bind (number end)
                    (parse-integer string
                                   :start i
                                   :junk-allowed t)
                  (setf i end)
                  number)))

(defun find-pattern (list &optional (minimum-length 5))
  (loop for length from minimum-length to (floor (/ (length list) 2))
        when (loop for i below length
                   for c-1 = (elt list i)
                   for c-2 = (elt list (+ i length))
                   always (= c-1 c-2))
          do (return-from find-pattern length)))

(uiop:define-package #:aoc/utils
  (:use #:cl)
  (:use-reexport #:alexandria #:serapeum #:split-sequence #:group-by)
  (:export
   #:read-input
   #:read-input-fields
   #:read-input-match
   #:char-number
   #:make-map))
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


(defun char-number (char)
  (- (char-int char) 48))

(defun make-map (input &key (value #'identity) delimiter)
  (loop with width = nil
        with data = nil
        for row = (read-line input nil)
        for height from 0
        while (and row (> (length row) 0))
        do (let ((fields (mapcar value (if delimiter
                                           (split-sequence delimiter row :test #'string=)
                                           (coerce row 'list)))))
             (unless width
               (setf width (length row)))
             (push fields data))
        finally (return (make-array (list height width)
                                    :initial-contents (nreverse data)))))

(defpackage #:aoc/day-20
  (:use #:cl #:aoc/utils)
  (:export #:day-20))
(in-package #:aoc/day-20)

(defclass module ()
  ((label
    :initarg :label
    :reader module-label)
   (targets
    :initarg :targets
    :accessor module-targets)
   (connected
    :initform nil
    :accessor module-connected)))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (format stream "~A" (module-label module))))

(defclass flip-flop (module)
  ((state
    :initform nil)))

(defmethod print-object ((module flip-flop) stream)
  (print-unreadable-object (module stream :type t)
    (with-slots (state)
        module
      (format stream "~A (~A)" (module-label module)
              (if state "on" "off")))))

(defclass conjunction (module)
  ((states
    :initform (make-hash-table))))

(defclass noop-module (module)
  ())

(defmethod pulse ((module module) sender type)
  (with-slots (targets)
      module
    (loop for target in targets
          collect (list module target type))))

(defmethod pulse ((module flip-flop) sender type)
  (with-slots (state)
      module
    (when (eq type :low)
      (setf state (not state))
      (call-next-method module sender (if state :high :low)))))

(defmethod pulse ((module conjunction) sender type)
  (with-slots (states connected)
      module
    (setf (gethash sender states) type)
    (call-next-method module sender (if (loop for from in connected
                                              for mem = (gethash from states)
                                              always (eq mem :high))
                                        :low
                                        :high))))

(defmethod pulse ((module noop-module) sender type))

(defun parse-line (line)
  (let* ((pos-source-end (position #\Space line))
         (source-type (case (aref line 0)
                        (#\% 'flip-flop)
                        (#\& 'conjunction)
                        (t 'module)))
         (source-label (subseq line
                               (if (eq source-type 'module) 0 1)
                               pos-source-end))
         (target-labels (loop for pos from (+ pos-source-end 4) below (length line)
                              for end = (or (position #\, line :start pos)
                                            (length line))
                              collect (subseq line pos end)
                              do (setf pos (1+ end)))))
    (make-instance source-type :label source-label :targets target-labels)))

(defun parse-input (input)
  (let ((modules (make-hash-table :test 'equal)))
    (loop for line = (read-line input nil)
          while line
          for module = (parse-line line)
          do (setf (gethash (module-label module) modules) module))
    (loop for module being the hash-value of modules
          do (setf (module-targets module)
                   (loop for target-label in (module-targets module)
                         for target-module = (or (gethash target-label modules)
                                                 (setf (gethash target-label modules)
                                                       (make-instance 'noop-module
                                                                      :label target-label)))
                         do (push module (module-connected target-module))
                         collect target-module)))
    modules))

(defun day-20 (input)
  (let* ((modules (parse-input input))
         (broadcast (gethash "broadcaster" modules)))
    (loop with low-count = 0
          with high-count = 0
          repeat 1000
          do (loop with next = (list (list nil broadcast :low))
                   while next
                   for next-next = (loop for (from to type) in next
                                         do (ecase type
                                              (:low (incf low-count))
                                              (:high (incf high-count)))
                                         nconc (pulse to from type))
                   do (setf next next-next))
          finally (return (* low-count high-count)))))

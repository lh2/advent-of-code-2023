(defpackage #:aoc/main
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:local-time)
  (:import-from #:dexador)
  (:nicknames #:aoc)
  (:export
   #:today
   #:run-day
   #:new-day
   #:main))
(in-package #:aoc/main)

(defconstant +year+ 2023)

(defvar *system* (asdf:find-system '#:aoc))

(defvar *cookie* nil)

(defun system-pathname (file)
  (asdf:system-relative-pathname *system* file))

(defun input-pathname (day)
  (system-pathname (format nil "input/~A.txt" day)))

(defun source-pathname (day)
  (system-pathname (format nil "src/day-~A.lisp" day)))

(defun test-pathname (day)
  (system-pathname (format nil "t/day-~A.lisp" day)))

(defun transform-input (input day)
  (typecase input
    (pathname
     (values
      (open input)
      #'close))
    (string
     (values
      (make-string-input-stream input)
      #'close))
    (stream
     (values
      input
      nil))
    (null
     (values
      (open (input-pathname day))
      #'close))))

(defun day-fun (day)
  (let* ((fun (format nil "DAY-~A" day))
         (package (format nil "AOC/~A" fun))
         (package (or (find-package package)
                      (progn
                        (asdf:load-system (string-downcase package))
                        (find-package package))))
         (fun (find-symbol fun package)))
    (symbol-function fun)))

(defun today ()
  (local-time:timestamp-day (local-time:now)))

(defun run-day (&optional (day (today)) input)
  (multiple-value-bind (input cleanup)
      (transform-input input day)
    (unwind-protect
         (funcall (day-fun day) input)
      (when cleanup
        (funcall cleanup input)))))

(defun new-day (&optional (day (today)))
  (let ((input-pathname (input-pathname day))
        (source-pathname (source-pathname day))
        (test-pathname (test-pathname day)))
    (ensure-directories-exist input-pathname)
    (ensure-directories-exist test-pathname)
    (unless (probe-file input-pathname)
      (restart-case
          (unless *cookie*
            (error "Advent Of Code cookie is unset"))
        (use-cookie (cookie)
          :interactive (lambda ()
                         (format t "Cookie: ")
                         (list (read-line)))
          (setf *cookie* cookie)))
      (alexandria:write-string-into-file
       (dex:get (format nil "https://adventofcode.com/~A/day/~A/input" +year+ day)
                :cookie-jar (cookie:make-cookie-jar
                             :cookies (list (cookie:make-cookie :name "session"
                                                                :value *cookie*
                                                                :domain ".adventofcode.com"))))
       input-pathname))
    (values
     (unless (probe-file source-pathname)
       (with-open-file (stream source-pathname :direction :output)
         (let ((fun (make-symbol (format nil "DAY-~A" day)))
               (package (make-symbol (format nil "AOC/DAY-~A" day)))
               (*print-case* :downcase))
           (format stream "~S~%~S~%~%(defun ~A (input)~%  )"
                   `(defpackage ,package
                      (:use #:cl #:aoc/utils)
                      (:export
                       ,fun))
                   `(in-package ,package)
                   fun)))
       ;; return function, makes it easy to jump into the file from Emacs
       (day-fun day))
     (unless (probe-file test-pathname)
       (let ((package (make-symbol (format nil "AOC-TEST/DAY-~A" day)))
             (*print-case* :downcase))
         (with-open-file (stream test-pathname :direction :output)
           (format stream "~S~%~S~%~%(define-test test-day-~A~%    ()~%  )"
                   `(defpackage ,package
                      (:use #:cl #:lisp-unit2))
                   `(in-package ,package)
                   day))
         (asdf:load-system "aoc-test/day-1")
         (symbol-function
          (find-symbol (format nil "TEST-DAY-~A" day)
                       (find-package package))))))))

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (today (or (first args)
                    (today)))
         (input (or (and #1=(second args)
                         (parse-namestring #1#))
                    (input-pathname today))))
    (dolist (task (multiple-value-list (run-day today input)))
      (format t "~A~%" task))))

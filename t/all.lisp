(defpackage #:aoc-test/all
  (:use #:cl #:lisp-unit2)
  (:nicknames #:aoc-test)
  (:import-from #:aoc-test/utils)
  (:export
   #:test-day
   #:test-all))
(in-package #:aoc-test/all)

(defun test-day (&optional (day (aoc:today)))
  (run-tests :package (format nil "AOC-TEST/DAY-~A" day)
             :run-contexts 'with-summary-context))

;; TODO: the asdf:load-system might not be so great here
(defun test-all ()
  (run-tests :tests (nconc
                     (get-tests :package '#:aoc-test/utils)
                     (loop for day from 1 to 25
                           for system = (format nil "aoc-test/day-~A" day)
                           nconc (handler-case
                                     (progn
                                       (asdf:load-system system)
                                       (get-tests :package (string-upcase system)))
                                   (asdf:missing-component (c)
                                     (declare (ignore c))))))
             :run-contexts 'with-summary-context))

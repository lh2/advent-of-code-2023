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

;; TODO: the recursive asdf/ql stuff might not be so great here
(defun test-all ()
  (run-tests :tests (nconc
                     (get-tests :package '#:aoc-test/utils)
                     (loop for day from 1 to 25
                           for system-name = (format nil "aoc-test/day-~A" day)
                           for system = (asdf:find-system system-name nil)
                           when system
                             do (ql:quickload system-name)
                             and nconc (get-tests :package (string-upcase system-name))))
             :run-contexts 'with-summary-context))

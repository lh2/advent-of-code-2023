#!/bin/sh
#|
exec sbcl --script "build.lisp"
|#
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(push (probe-file #P".") asdf:*central-registry*)
(ql:quickload :aoc)
(loop for day from 1 to 25
      do (handler-case
             (asdf:load-system (format nil "aoc/day-~A" day))
           (asdf:missing-component (c)
             (declare (ignore c)))))
(sb-ext:save-lisp-and-die "aoc"
                          :toplevel #'aoc:main
                          :executable t)

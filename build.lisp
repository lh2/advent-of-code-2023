#!/bin/sh
#|
exec sbcl --script "build.lisp"
|#
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(push (probe-file #P".") asdf:*central-registry*)

(sb-ext:restrict-compiler-policy 'speed 3 3)
(push :release *features*)

(ql:quickload :aoc)
(loop for day from 1 to 25
      for system-name = (format nil "aoc/day-~A" day)
      for system = (asdf:find-system system-name nil)
      when system
        do (ql:quickload system-name))
(sb-ext:save-lisp-and-die "aoc"
                          :toplevel #'aoc:main
                          :executable t)

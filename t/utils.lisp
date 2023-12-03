(uiop:define-package #:aoc-test/utils
  (:use #:cl)
  (:mix #:lisp-unit2 #:aoc/utils))
(in-package #:aoc-test/utils)

(define-test test-read-input
    ()
  (with-input-from-string (stream "hello
world")
    (assert-equalp '("hello" "world")
                   (read-input stream)))
  (with-input-from-string (stream "1
2
3

3
2
1")
    (assert-equalp '(1 2 3 nil 3 2 1)
                   (read-input stream :type 'integer)))
  (with-input-from-string (stream "this
does
not
matter")
    (assert-equalp '(1 1 1 1)
                   (read-input stream :type (lambda (line)
                                              (declare (ignore line))
                                              1)))))

(define-test test-read-input-fields
    ()
  (with-input-from-string (stream "A 1
B 2
C 3
D
E 5

G 7")
    (assert-equalp '(("A" 1)
                     ("B" 2)
                     ("C" 3)
                     ("D" nil)
                     ("E" 5)
                     (nil nil)
                     ("G" 7))
                   (read-input-fields stream '(string integer)))))

(define-test test-read-input-match
    ()
  (with-input-from-string (stream "x: 1, y: 2
x: 3, y: 4
x: 2, y: 5")
    (assert-equalp '(("x" 1 "y" 2)
                     ("x" 3 "y" 4)
                     ("x" 2 "y" 5))
                   (read-input-match stream
                                     "(\\w+): (\\d+), (\\w+): (\\d+)"
                                     :types '(string integer string integer)))))

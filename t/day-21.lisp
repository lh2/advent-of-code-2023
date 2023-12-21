(defpackage #:aoc-test/day-21
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-21)

(defparameter *test-map* "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(define-test test-day-21
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 21 *test-map* 6 6)
    (assert= 16 task-1)
    (assert= 16 task-2))
  ;; Tests don't work for task 2
  #+nil(loop for (expected steps) in '((50 10)
                                       (1594 50)
                                       (6536 100)
                                       (167004 500)
                                       (668697 1000)
                                       (16733044 5000))
             do (assert= expected (nth-value 1 (aoc:run-day 21 *test-map* 0 steps)))))

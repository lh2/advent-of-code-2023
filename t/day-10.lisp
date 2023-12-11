(defpackage #:aoc-test/day-10
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-10)

(define-test test-day-10
    ()
  (multiple-value-bind (task-1)
      (aoc:run-day 10 ".....
.S-7.
.|.|.
.L-J.
.....")
    (assert= 4 task-1))
  (multiple-value-bind (task-1)
      (aoc:run-day 10 "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")
    (assert= 8 task-1))

  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 10 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")
    (declare (ignore task-1))
    (assert= 4 task-2))
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 10 "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........")
    (declare (ignore task-1))
    (assert= 4 task-2))
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 10 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")
    (declare (ignore task-1))
    (assert= 8 task-2))
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 10 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")
    (declare (ignore task-1))
    (assert= 10 task-2)))

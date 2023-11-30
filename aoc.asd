(defsystem "aoc"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("aoc/main")
  :in-order-to ((test-op (load-op "aoc-test")))
  :perform (test-op (o c) (symbol-call :aoc-test :test-all)))

;;; Transpiler module tests.

(in-package :davis-test.transpiler)

(def-suite transpiler)

(in-suite transpiler)

(test a
  ;; (finishes (davis.transpiler:transpile-tree
  ;;             (davis.parser:parse-file ".archive/examples/lotto.pcode")))
  ;; (finishes (davis.transpiler:transpile-tree
  ;;             (davis.parser:parse-file ".archive/examples/arithmetic.pcode")))
  (finishes (davis.transpiler:transpile-tree
              (davis.parser:parse-file ".archive/examples/hello_world.pcode"))))

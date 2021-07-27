;;;; davis.asd

(asdf:defsystem :davis
  :description "An SDD Pseudocode implementation"
  :author "Shunyao Liang <shunyao.liang@education.nsw.gov.au>"
  :license "MIT"
  :version "0.0.4"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parser")
                             (:file "transpiler")
                             (:file "playground")
                             (:file "davis"))))
  :depends-on (:alexandria
               :asdf
               :binding-arrows
               :cffi
               :esrap
               :lucerne
               :mmap
               :parse-number
               :trivial-dump-core)
  :entry-point "davis:main"
  :build-operation "program-op")

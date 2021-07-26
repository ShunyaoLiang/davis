;;;; davis.asd

(asdf:defsystem :davis
  :description "An SDD Pseudocode implementation"
  :author "Shunyao Liang <shunyao.liang@education.nsw.gov.au>"
  :license "MIT"
  :version "0.0.3"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parser")
                             (:file "transpiler"))))
  :depends-on (:alexandria
               :binding-arrows
               :cffi
               :esrap
               :mmap
               :parse-number))

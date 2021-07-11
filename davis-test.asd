;;;; davis-test.asd

(asdf:defsystem :davis-test
  :description "Test suites for Davis, an SDD Pseudocode implementation"
  :author "Shunyao Liang <shunyao.liang@education.nsw.gov.au>"
  :license "Specify license here"
  :version "0.0.1"
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "parse"))))
  :depends-on (:davis :fiveam))
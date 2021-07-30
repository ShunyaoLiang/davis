;;;; davis.asd

(asdf:defsystem :davis
  :description "An SDD Pseudocode implementation"
  :author "Shunyao Liang <shunyao.liang@education.nsw.gov.au>"
  :license "MIT"
  :version "0.1.0"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utilities")
                             (:file "parser")
                             (:file "transpiler")
                             (:file "davis")  
                             (:file "playground")))
               (:module "playground"
                :components ((:module "images/favicon"
                              :components ((:static-file "favicon-16x16.png")
                                           (:static-file "favicon-32x32.png")))
                             (:module "scripts"
                              :components ((:static-file "jquery.js")
                                           (:static-file "caret.js")
                                           (:static-file "main.js")))
                             (:module "styles"
                              :components ((:static-file "style.js")))
                             (:module "templates"
                              :components ((:static-file "index.html"))))))
  :depends-on (:alexandria
               :asdf
               :binding-arrows
               :cffi
               :esrap
               :lucerne
               :mmap
               :parse-number
               :sb-concurrency
               :trivial-dump-core
               :websocket-driver)
  :entry-point "davis:main"
  :build-operation "program-op")

;;;; davis.asd

(asdf:defsystem :davis
  :description "A SDD Pseudocode implementation"
  :author "Shunyao Liang <shunyao.liang@education.nsw.gov.au>"
  :license "MIT"
  :version "0.0.1"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "parse"))))
  :depends-on (:alexandria
               :binding-arrows
               :cffi
               :esrap
               :mmap
               :parse-number))

;  :depends-on (#:esrap #:parse-number #:alexandria #:access #:ceramic #:lucerne #:djula)
;  :components ((:file "package")
;               (:file "davis")
;               (:file "internal")
;               (:file "piece-table")
;               (:file "playground"))
;  :build-operation program-op
;  :build-pathname "davis"
;  :entry-point "davis:main" 

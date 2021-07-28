(defpackage :davis.utilities
  (:use :cl)
  (:export :eval-muffling-warnings :find-entry-point :print-conditions :string-case))

(defpackage :davis.parser
  (:use :binding-arrows :cl :esrap :parse-number)
  (:import-from :alexandria :rcurry :removef :switch)
  (:import-from :cffi :foreign-string-to-lisp)
  (:import-from :mmap :with-mmap)
  (:shadow :< :> :<= :>=)
  (:export :parse-file :parse-pseudocode :*program-arrays*))

(defpackage :davis.transpiler
  (:use :binding-arrows :cl)
  (:import-from :alexandria :if-let :curry)
  (:import-from :davis.parser :*program-arrays*)
  (:import-from :davis.utilities :string-case)
  (:export :transpile-tree))

(defpackage :davis
  (:use :binding-arrows :cl)
  (:import-from :alexandria :rcurry :switch)
  (:import-from :davis.parser :parse-file)
  (:import-from :davis.transpiler :transpile-tree)
  (:import-from :davis.utilities
                :eval-muffling-warnings
                :find-entry-point
                :print-conditions
                :string-case)
  (:import-from :trivial-dump-core :save-executable)
  (:import-from :uiop :command-line-arguments)
  (:export :main))

(defpackage :davis.playground
  (:use :binding-arrows :cl :lucerne)
  (:import-from :alexandria :read-stream-content-into-string)
  (:import-from :annot :enable-annot-syntax)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :davis.parser :parse-pseudocode)
  (:import-from :davis.transpiler :transpile-tree)
  (:import-from :davis.utilities :eval-muffling-warnings :find-entry-point :print-conditions)
  (:import-from :djula :add-template-directory :compile-template*)
  (:import-from :hunchentoot :raw-post-data)
  (:export :app))

(defpackage :davis.user)

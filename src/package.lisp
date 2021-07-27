(defpackage :davis.parser
  (:use :binding-arrows :cl :esrap :parse-number)
  (:import-from :alexandria :switch)
  (:import-from :cffi :foreign-string-to-lisp)
  (:import-from :mmap :with-mmap)
  (:export :parse-file :parse-pseudocode))

(defpackage :davis.transpiler
  (:use :binding-arrows :cl)
  (:export :transpile-tree))

(defpackage :davis
  (:use :binding-arrows :cl)
  (:import-from :alexandria :switch)
  (:import-from :davis.parser :parse-file)
  (:import-from :davis.transpiler :transpile-tree)
  (:import-from :trivial-dump-core :save-executable)
  (:import-from :uiop :command-line-arguments)
  (:export :main :print-conditions))

(defpackage :davis.playground
  (:use :binding-arrows :cl :lucerne)
  (:import-from :alexandria :read-stream-content-into-string)
  (:import-from :annot :enable-annot-syntax)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :davis :print-conditions)
  (:import-from :davis.parser :parse-pseudocode)
  (:import-from :davis.transpiler :transpile-tree)
  (:import-from :djula :add-template-directory :compile-template*)
  (:import-from :hunchentoot :raw-post-data)
  (:export :app))

(defpackage :davis.user)

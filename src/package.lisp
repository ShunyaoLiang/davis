(defpackage :davis.parser
  (:use :binding-arrows :cl :esrap :parse-number)
  (:import-from :alexandria :switch)
  (:import-from :cffi :foreign-string-to-lisp)
  (:import-from :mmap :with-mmap)
  (:export :parse-file :parse-pseudocode))

(defpackage :davis.transpiler
  (:use :binding-arrows :cl)
  (:export :transpile-tree))

(defpackage :davis.playground
  (:use :cl :lucerne)
  (:import-from :annot :enable-annot-syntax)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :djula :add-template-directory :compile-template*)
  (:export :app))

(defpackage :davis
  (:use :binding-arrows :cl)
  (:import-from :alexandria :switch)
  (:import-from :davis.parser :parse-file)
  (:import-from :davis.transpiler :transpile-tree)
  (:import-from :trivial-dump-core :save-executable)
  (:import-from :uiop :command-line-arguments)
  (:export :main))

(defpackage :davis.user)

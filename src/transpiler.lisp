;;;; Generate Lisp forms from syntax trees.

(in-package :davis.transpiler)

;;; Primary Interface

(defun transpile-tree (tree)
  (->> tree
       (mapcar #'transpile-node)
       (mapc #'print)
       (mapc #'eval)))

;;; Functions

(defun transpile-node (node)
  (-<> (getf node :type)
       (string)
       (concatenate 'string "TRANSPILE-" <>)
       (intern :davis.transpiler)
       (symbol-function)
       (apply (getf node :fields))))

(defun transpile-procedure (&key name parameters statements local-bindings arrays)
  (declare (ignore arrays))
  `(defun ,name ,parameters
     (let ,local-bindings
       (block nil
              ,@(mapcar #'transpile-node statements)))))

(defun transpile-display-statement (&rest items)
  `(format t "~{~a~^ ~}~&" (list ,@(mapcar #'transpile-node items))))

(defun transpile-literal (literal)
  literal)

;;;; Generate Lisp forms from syntax trees.

(in-package :davis.transpiler)

;;; Globals

(defvar *filespec* nil
  "The file specification of the tree being transpiled.")

;;; Primary Interface

(defun transpile-tree (tree &optional filespec)
  (setf *filespec* filespec)
  (mapcar #'transpile-node tree))

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
     (block nil
       (let ,local-bindings
         ,@(mapcar #'transpile-node statements)))))

(defun transpile-display-statement (&rest items)
  `(format t "~{~a~^ ~}~&" (list ,@(mapcar #'transpile-node items))))

(defun transpile-literal (literal)
  literal)

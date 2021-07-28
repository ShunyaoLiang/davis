;;;; Generate Lisp forms from syntax trees.

(in-package :davis.transpiler)

;;; Globals

(defvar *filespec* nil
  "The file specification of the tree being transpiled.")

(defvar *procedure-arguments* nil
  "The arguments of the procedure being transpiled.")

(defvar *program-array-symbols* nil
  "The symbols of the arrays of the program being transpiled.")

(defvar *node-meta* nil
  "The :META field of the node being transpiled.")

;;; Macros

(defmacro box (place)
  ;; Do not box literals.
  (if (or (typep place 'number)
          (typep place 'string)
          (eq place 't)
          (eq place 'nil))
      `(lambda () ,place)
      `(lambda (&optional v)
         (if v (setf ,place v) ,place))))

(defmacro guard (meta &rest form)
  "Amend all errors thrown in FORM to hold their textual metadata."
  `(handler-case (progn ,@form)
     (error (error) (error 'runtime-error :internal error :meta ',meta))))

;;; Primary Interface

(defun transpile-tree (tree &optional filespec)
  (setf *filespec* filespec
        *procedure-arguments* nil
        *program-array-symbols* nil)
  (->> tree
       (mapc #'collect-arrays)
       (mapc #'bind-dummy-function)
       (transpile-nodes)))

;;; Functions

(defun transpile-node (node)
  (let ((*node-meta* (or (getf node :meta) *node-meta*))) ; Don't shadow with NIL.
    (-<> (getf node :type)
         (string)
         (concatenate 'string "TRANSPILE-" <>)
         (intern :davis.transpiler)
         (symbol-function)
         (apply (getf node :fields)))))

(defun transpile-procedure (&key name parameters statements local-bindings arrays)
  (declare (ignore arrays))
  (if-let (intersection (intersection parameters *program-arrays*))
    (error 'parameter-shadows-array-error :intersection intersection))
  (let ((*procedure-arguments* parameters))
    `(defun ,name ,parameters
       (block nil
         (let ,local-bindings
           ,@(transpile-nodes statements))))))

(defun transpile-display-statement (&rest items)
  `(format t "~{~a~^ ~}~&" (preprint (list ,@(transpile-nodes items)))))

(defun preprint (objs)
  (loop for obj in objs
        collect (cond ((eq t obj) "true")
                      ((eq nil obj) "false")
                      ((and (typep obj 'array)
                            (not (typep obj 'string)))
                       (format nil "[~{~a~^,~^ ~}]" (coerce obj 'list)))
                      (t obj))))

(defun transpile-return-statement (value)
  `(return ,(transpile-node value)))

(defun transpile-let-statement (&key lhs rhs)
  (let ((lhs (if (consp lhs) (transpile-node lhs) lhs)) ; Only TRANSPILE-NODE if it is an assignment to an array access.
        (rhs (transpile-node rhs)))
    ;; Is this a parameter modification?
    (cond ((and (consp lhs) (eq (car lhs) 'funcall))
           (append lhs (list rhs))) ; It is already being unboxed; i.e. it is treating a parameter as an array.
          ((member lhs *procedure-arguments*)
           `(funcall ,lhs ,rhs))
          (t `(setf ,lhs ,rhs)))))

(defun transpile-get-statement (&rest arguments)
  (flet ((make-receiver (symbol)
           `(progn (format t "~a: " ,(string symbol))
                   (setf ,symbol (read)))))
    (if (> (length arguments) 1)
        `(loop for argument in ,arguments
           do ,@(cdr (make-receiver 'argument))) ; The PROGN is unecessary in LOOP.
        (make-receiver (first arguments)))))

(defun transpile-open-statement (&key filespec mode)
  (list 'setf filespec
        (list 'open (string filespec)
              :direction (string-case mode
                           ("input" :input)
                           (("output" "append") :output)
                           ("relative access" :io))
              :if-exists (string-case mode
                           (("output" "relative access") :overwrite)
                           ("append" :append))
              :if-does-not-exist :create)))

;; TODO: Define file I/O protocol.
(defun transpile-read-statement (&key identifiers filespec)
  (declare (ignore identifiers filespec))
  '(error "File I/O is unimplemented :("))

(defun transpile-write-statement (&key identifiers filespec)
  (declare (ignore identifiers filespec))
  '(error "File I/O is unimplemented :("))

(defun transpile-close-statement (filespec)
  `(close ,filespec))

(defun transpile-dim-statement (name dimensions type)
  (declare (ignore type))
  `(defparameter ,name (make-array ',dimensions :element-type t)))

(defun transpile-if-statement (condition then-block else-block)
  `(if ,(transpile-node condition)
       (progn :then ,@(transpile-nodes then-block))
       (progn :else ,@(transpile-nodes else-block))))

(defun transpile-casewhere-statement (test choices otherwise-process)
  `(alexandria:switch (,(transpile-node test) :test #'equalp)
     ,@(loop for choice in choices
             collect `(,(transpile-node (first choice))                       ; The condition.
                        (progn ,@(transpile-nodes (second choice))))) ; Transpile inner blocks
     (t ,@(transpile-nodes otherwise-process))))

(defun transpile-while-statement (condition statements)
  `(loop while ,(transpile-node condition)
         do ,@(transpile-nodes statements)))

(defun transpile-for-statement (counter init-value finish-value step-value statements)
  `(loop with ,counter = ,(transpile-node init-value)
         until (equalp ,counter ,(transpile-node finish-value))
         do ,@(transpile-nodes statements)
         (incf ,counter ,(if step-value ; There are cases where the step-value is omitted.
                             (transpile-node step-value)
                             1))))

(defun transpile-repeat-statement (condition statements)
  `(loop do ,@(transpile-nodes statements)
         until ,(transpile-node condition)))

(defun transpile-array-access-or-procedure-call (&key identifier arguments)
  ;; Is this an array access?
  (let ((subscripts (mapcar (curry #'list '1-) (transpile-nodes arguments))))
    (cond ((member identifier *program-array-symbols*)
           `(aref ,identifier ,@subscripts))
          ((member identifier *procedure-arguments*)
           `(aref (funcall ,identifier) ,@subscripts))
          ;; Otherwise, procedure call.
          (t `(,identifier ,@(loop for argument in (transpile-nodes arguments)
                                   collect `(box ,argument)))))))

(defun transpile-binary-operation (&key operator lhs rhs)
  `(guard ,*node-meta* (,operator ,(transpile-node lhs) ,(transpile-node rhs))))

(defun transpile-unary-operation (&key operator operand)
  `(guard ,*node-meta* (,operator ,(transpile-node operand))))

(defun transpile-record (&key name slots)
  `(defstruct ,name
     ,@(loop for slot in slots
             for dimensions = (getf slot :dimensions)
             collect (list (getf slot :name)
                           (if dimensions
                               `(make-array ,dimensions)
                               nil)))))

(defun transpile-comment ()
  nil)

(defun transpile-binding (binding)
  ;; If the binding is a parameter, it must be 'unboxed'.
  (if (member binding *procedure-arguments*)
      `(funcall ,binding)
      binding))

(defun transpile-literal (literal)
  literal)

(defun collect-arrays (form)
  (when (eq (getf form :type) :procedure)
    (setf *program-array-symbols*
          (append *program-array-symbols* (getf (getf form :fields) :arrays)))))

(defun transpile-nodes (nodes)
  (->> nodes
       (remove nil)
       (mapcar #'transpile-node)))

(defun bind-dummy-function (form)
  (when (eq (getf form :type) :procedure)
    (setf (symbol-function (getf (getf form :fields) :name))
          (lambda (&rest _) (declare (ignore _))))))

;;; Errors

(define-condition runtime-error (error)
  ((internal :initarg :internal)
   (meta :initarg :meta))
  (:report (lambda (condition stream)
             (with-slots (internal meta) condition
               (format stream "~a at ~a" internal meta)))))

(define-condition parameter-shadows-array-warning (error)
  ((intersection :initarg :intersection))
  (:report (lambda (condition stream)
             (with-slots (intersection) condition
               (format stream "Array with same name as parameter: ~a" intersection)))))

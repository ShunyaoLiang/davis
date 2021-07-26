;;;; The mainline; handle command-line arguments.

(in-package :davis)

;;; Macros

(defmacro string-case (keyform &rest cases)
  "CASE for string-matching."
  (let ((test (gensym "TEST")))
    (flet ((transform-clause (clause)
             (destructuring-bind (keys forms) clause
               (declare (ignore forms))
               (rplaca clause (cond ((consp keys) `(member ,test ',keys :test #'string=))
                                    ((eq keys 'otherwise) 't)
                                    (t `(string= ,test ,keys)))))))
      `(let ((,test ,keyform))
         (cond ,@(mapcar #'transform-clause cases)))))) 

;;; Primary Interface

(defun main ()
  (if (zerop (length (command-line-arguments)))
      (display-help)
      (destructuring-bind (command &rest arguments) (command-line-arguments)
        (declare (ignore arguments))
        (string-case command
                     (("i" "interpret") (interpret arguments))
                     (otherwise (display-help))))))

;;; Functions

(defun interpret (filespecs)
  (loop for filespec in filespecs
        do (-<> filespec
                (parse-file)
                (transpile-tree filespec)
                (mapc #'eval <>)))
  ;; Find the entry-point.
  (macrolet ((find-function (symbol) `(handler-case (symbol-function ,symbol) (type-error () nil))))
    (funcall (or
               (find-function 'davis.user::mainprogram)
               (error 'no-entry-point-error)))))

(defun display-help ()
  (format t "Usage:~%~
             ~{    davis ~a~&~}~&"
          (list "[i | interpret] filespecs..."
                "[c | compile] filespecs..."
                "[p | playground]")))

;;; Errors

(define-condition no-entry-point-error (error)
  ()
  (:report "No entry point found. Ensure you have a procedure named MAINPROGRAM."))

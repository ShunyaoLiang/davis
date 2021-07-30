;;;; Various minor functions and macros.

(in-package :davis.utilities)

;;; Macros

(defmacro string-case (keyform &rest cases)
  "CASE for string-matching."
  (let ((test (gensym "TEST")))
    (flet ((transform-clause (clause)
             (destructuring-bind (keys &rest forms) clause
               (declare (ignore forms))
               (rplaca clause (cond ((consp keys) `(member ,test ',keys :test #'string=))
                                    ((eq keys 'otherwise) 't)
                                    (t `(string= ,test ,keys)))))))
      `(let ((,test ,keyform))
         (cond ,@(mapcar #'transform-clause cases))))))

(defmacro print-conditions (&body form)
  "Print any conditions that are thrown in FORM and continue."
  `(handler-case (progn ,@form)
     (error (error) (write error :escape nil))))

(defmacro movef (place new-value)
  "Sets PLACE to be NEW-VALUE, returning the previous value of PLACE."
  `(prog1 ,place (setf ,place ,new-value)))

;;; Functions

(defun find-entry-point ()
  "Find the entry point function in DAVIS.USER of the user's Pseudocode."
  (macrolet ((find-function (symbol)
               `(handler-case (symbol-function ,symbol) (type-error () nil))))
    (or (find-function 'davis.user::mainprogram)
        (error 'no-entry-point-error))))

(defun eval-muffling-warnings (original-exp)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (eval original-exp)))

;;;; The mainline; handle command-line arguments.

(in-package :davis)

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

;;; Primary Interface

(defun main ()
  (if (zerop (length (command-line-arguments)))
      (display-help)
      (destructuring-bind (command &rest arguments) (command-line-arguments)
        (string-case command
          (("i" "interpret") (load-files arguments)
                             (funcall (find-entry-point)))
          (("c" "compile") (load-files arguments)
                           ;; TODO: Add support for a -o flag.
                           (save-executable "a.out" (find-entry-point)))
          (("p" "playground") (start-playground))
          (otherwise (display-help))))))

;;; Functions

(defun load-files (filespecs)
  (loop for filespec in filespecs
        do (-<> filespec
                (parse-file)
                (transpile-tree filespec)
                (mapc #'eval <>))))

(defun start-playground (&optional (port 32847))
  (lucerne:start davis.playground:app :port port :silent t :debug t)
  (format t "Playground started at https://localhost:~a/.~&~
             Type 'exit' to turn off the playground.~&" port)
  ;; TODO: Try to automatically open the user's browser.
  (loop until (string= (read-line) "exit"))
  (write-line "Co'o")
  (lucerne:stop davis.playground:app))

(defun display-help ()
  (format t "Usage:~%~
             ~{    davis ~a~&~}~&"
          (list "[i | interpret] filespecs..."
                "[c | compile] filespecs..."
                "[p | playground]")))

;;; Utilities

(defun find-entry-point ()
  (macrolet ((find-function (symbol)
               `(handler-case (symbol-function ,symbol) (type-error () nil))))
    (or (find-function 'davis.user::mainprogram)
        (error 'no-entry-point-error))))

;;; Errors

(define-condition no-entry-point-error (error)
  ()
  (:report "No entry point found. Ensure you have a procedure named MAINPROGRAM."))

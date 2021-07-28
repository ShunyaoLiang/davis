;;;; The mainline; handle command-line arguments.

(in-package :davis)

;;; Primary Interface

(defun main ()
  ;; Initialise the random state.
  (setf *random-state* (make-random-state t))
  
  (if (zerop (length (command-line-arguments)))
      (display-help)
      (destructuring-bind (command &rest arguments) (command-line-arguments)
        (print-conditions
          (string-case command
            (("i" "interpret") (load-files arguments)
                               (funcall (find-entry-point)))
            (("c" "compile") (load-files arguments)
                             ;; TODO: Add support for a -o flag.
                             (save-executable "a.out" (find-entry-point)))
            (("p" "playground") (start-playground))
            (otherwise (display-help)))))))

;;; Functions

(defun load-files (filespecs)
  ;; Muffle style warnings as they usually are about the generated code.
  (loop for filespec in filespecs
        do (-<> filespec
                (parse-file)
                (transpile-tree filespec)
                ;(mapc (rcurry #'print *error-output*) <>)
                (mapc #'eval-muffling-warnings <>))))

(defun start-playground (&optional (port 32847)) ; 32847 is DAVIS on a phone keypad :)
  (lucerne:start davis.playground:app :port port :silent t :debug t)
  (format t "Playground started at https://localhost:~a/.~&~
             Type 'exit' to turn off the playground.~&" port)
  ;; TODO: Try to automatically open the user's browser.
  (loop until (string= (read-line) "exit"))
  (write-line "co'o")
  (lucerne:stop davis.playground:app))

(defun display-help ()
  (format t "Usage:~%~
             ~{    davis ~a~&~}~&"
          (list "[i | interpret] filespecs..."
                "[c | compile] filespecs..."
                "[p | playground]")))

(defun davis.user::|Random| (high)
  (random (1+ (funcall high))))

;;; Errors

(define-condition no-entry-point-error (error)
  ()
  (:report "No entry point found. Ensure you have a procedure named MAINPROGRAM."))

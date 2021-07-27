;;;; Host a web application that interprets Pseudocode.

(in-package :davis.playground)

(enable-annot-syntax)

;;; App

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>
                 (clack.middleware.static:<clack-middleware-static>
                   :path "/static/"
                   :root (system-relative-pathname :davis #p"playground/"))))

(add-template-directory (system-relative-pathname :davis #p"playground/templates/"))

;;; Templates

(defparameter +index+ (compile-template* "index.html")
  "The index page template.")

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+)))

@route app (:post "/evaluate/")
(defview evaluate ()
  (let ((pseudocode (read-stream-content-into-string (raw-post-data)))
        (*standard-output* (make-string-output-stream)))
    (print-conditions
      (->> pseudocode
           (parse-pseudocode)
           (transpile-tree)
           (mapc #'eval))
      ;; TODO: Abstract into an executor module perhaps?
      (funcall (find-entry-point)))
    (respond (get-output-stream-string *standard-output*))))

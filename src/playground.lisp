;;;; Host a web application that interprets Pseudocode.

(in-package :davis.playground)

(enable-annot-syntax)

;;; App

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>
                 (clack.middleware.static:<clack-middleware-static>
                   :path "/static/"
                   :root (system-relative-pathname :davis #p"playground/static/"))))

(add-template-directory (system-relative-pathname :davis #p"playground/templates/"))

;;; Templates

(defparameter +intro+ (compile-template* "intro.html")
  "The intro screen template.")

(defparameter +editor+ (compile-template* "editor.html")
  "The editor screen template.")

;;; Views

@route app "/"
(defview index ()
  (redirect "/intro"))

@route app "/intro"
(defview intro ()
  (render-template (+intro+)))

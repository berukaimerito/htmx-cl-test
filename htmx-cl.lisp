;;;; htmx-cl.lisp

(in-package #:htmx-cl)

(defvar *server* nil "The Hunchentoot server instance")

(defparameter *htmx-test-project* #P"/home/bkc/code/htmx-cl/")

(defmacro with-html-page (&body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "UTF-8")
       (:meta :name "viewport" :content "width=device-wdith, initial-scale=1.0")
       (:title "HTMX Common Lisp Demo")
       (:script :src "https://unpkg.com/htmx.org@2.0.4"
                :crossorigin "anonymous"))
     (:body
      ,@body))))

(defmacro with-button (endpoint target image-src name &optional (color "black") &body extra-content)
  `(with-html-string
     (:div
      (:button :hx-post ,endpoint
               :hx-target ,target
               :style "margin-right: 5px; display: inline-block;"
               (:img :src ,image-src :width "20" :height "20" :style "margin-right: 5px;")
               (:font :color ,color ,name))
      ,@extra-content)))

; Define a route for the main page
(define-easy-handler (index :uri "/") ()
  (setf (content-type*) "text/html")
  (with-html-page
    (:h1 "Question")
    (:div :style "display: flex; justify-content: flex-start; margin-bottom: 10px;"
     (:raw (with-button "/btn1" "#result" "/images/lisp.png" "Common Lisp"))
     (:raw (with-button "/btn2" "#result" "/images/lisp.png" "Common Lisp"))
     (:raw (with-button "/btn3" "#result" "/images/lisp.png" "Common Lisp"))
     )
    (:div :id "result" "Results will appear here")))

;; Define an HTMX endpoint
(define-easy-handler (hello :uri "/btn1") ()
  (setf (content-type*) "text/html")
  (with-html-string
    (:div :class "result-message"
          (:p (:font :color "green" (:br) "Common Lisp via HTMX!"))
          )))

;; Server control functions
(defun start-server (&optional (port 8080))
  "Start the web server on the specified port"
  (when *server*
    (stop-server))
  (setf *server* (make-instance 'easy-acceptor :port port))
  (push (create-folder-dispatcher-and-handler
         "/images/"
         (merge-pathnames #P"images/" *htmx-test-project*))
        *dispatch-table*)
  (start *server*)
  (format t "~&Server started on port ~D~%" port)
  *server*)

(defun stop-server ()
  "Stop the web server"
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "~&Server stopped~%"))
  nil)

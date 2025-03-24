;;;; htmx-cl.lisp

(in-package #:htmx-cl)

(defvar *project-path* )

(defvar *server* nil "The Hunchentoot server instance")

(defparameter *mascots-hash*
  (alexandria:alist-hash-table
   '(("zig" . nil)
     ("lisp" . nil)
     ("cpp" . nil)
     ("rust" . nil)
     ("java" . nil)
     ("golang" . nil)
     ("php" . nil)
     ("raku" . nil)
     )
   :test 'equal))

(defparameter *winners* '())

(defun pick-pairs ()
  (let ((found '())
        (all-used t))
    ;; First check if all values are already used
    (maphash (lambda (k v) (when (null v) (setf all-used nil))) *mascots-hash*)

    (when (and (= 1 (length *winners*)) all-used)
      (return-from pick-pairs (list (first *winners*) nil)))

    ;; TODO - When all used the winner is length of one just serve the winner endpoint.
    (when all-used
      (mapcar (lambda (el) (setf (gethash el *mascots-hash*) nil)) *winners*)
      (setf *winners* nil))

    ;; TODO - Randomize order
    (loop for (key . value) in (alexandria:hash-table-alist *mascots-hash*)
          unless value
            do (setf (gethash key *mascots-hash*) t)
               (push key found)
          when (= (length found) 2)
            do (return))
    found))

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

(defmacro with-button (endpoint target image-src &optional name (color "black") &body extra-content)
  `(with-html-string
     (:div
      (:button :hx-post ,endpoint
               :hx-target ,target
               :style "margin-right: 10px; display: inline-block; padding: 15px; min-width: 100px; min-height: 100px;"
               (:img :src ,image-src :width "100" :height "80" :style "margin-right: 10px;")
               (:font :color ,color ,name))
      ,@extra-content)))

(defun change-pairing ()
  (let* ((pairs (pick-pairs))
         (mascot1 (first pairs))
         (mascot2 (second pairs)))

    (if (not mascot2)
        (with-html-string
          (:div (format nil "~a" mascot1)))
        (with-html-string
          (:div :id "buttons" :style "display: flex; justify-content: center; margin-bottom: 10px;"
            (:raw (with-button (format nil "select?mascot=~A" mascot1)
                    "#buttons" (format nil "/images/~a.png" mascot1)))
            (:raw (with-button (format nil "select?mascot=~A" mascot2)
                    "#buttons" (format nil "/images/~A.png" mascot2))))))))

; Define a route for the main page
(define-easy-handler (index :uri "/") ()
  (setf (content-type*) "text/html")
  (with-html-page
    (:h1 "Question")
    (:h2 "Which programming mascot is the best?")
    (:div :id "result" "Results will appear here")
    (:raw (change-pairing))))


(defun pick-the-mascot-game (mascot)
    (push mascot *winners*)
    (format t "~&Selected: ~A~%" mascot)
    (change-pairing))


;; Server control functions
(defun start-server (&optional (port 8080))
  "Start the web server on the specified port"
  (when *server*
    (stop-server))
  (setf *server* (make-instance 'easy-acceptor :port port))
  (push (create-folder-dispatcher-and-handler
         "/images/"
         (merge-pathnames #P"images/"  #P"/home/bkc/code/htmx-cl/"))
        *dispatch-table*)
  (start *server*)
  (format t "~&Server started on port ~D~%" port)
  *server*)

(define-easy-handler (select-mascot :uri "/select") (mascot)
  (setf (content-type*) "text/html")
  ;; Store the selection if provided
  (when mascot
    (pick-the-mascot-game mascot)
    ))

(defun stop-server ()
  "Stop the web server"
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "~&Server stopped~%"))
  nil)

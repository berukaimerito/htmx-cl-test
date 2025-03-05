;;;; htmx-cl.asd

(asdf:defsystem #:htmx-cl
  :description "Describe htmx-cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:ningle #:spinneret #:local-time)
  :components ((:file "package")
               (:file "htmx-cl")))

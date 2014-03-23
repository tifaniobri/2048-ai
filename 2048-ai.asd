;;;; 2048-ai.asd

(asdf:defsystem #:2048-ai
  :serial t
  :description "Describe 2048-ai here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:selenium #:iterate #:alexandria)
  :components ((:file "package")
               (:file "selenium")
               (:file "board")
               (:file "moves")
               (:file "2048-ai")))


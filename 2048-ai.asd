;;;; 2048-ai.asd

(asdf:defsystem #:2048-ai
  :serial t
  :description "An ai to play 2048"
  :author "Trent Summerfield <trent.summerfield@gmail.com>"
  :license "MIT"
  :depends-on (#:selenium #:iterate #:alexandria)
  :components ((:file "package")
               (:file "selenium")
               (:file "board")
               (:file "moves")
               (:file "2048-ai")))


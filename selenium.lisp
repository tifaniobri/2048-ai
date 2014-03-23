(in-package #:2048-ai)

(defun start-interactive-session ()
  (setf selenium:*SELENIUM-SESSION*
        (selenium:do-get-new-browser-session "*googlechrome"
                                             "http://localhost:8000"))
  (selenium:do-open "http://localhost:8000"))

(defun encode-move (move)
  (case move
    (up "0")
    (right "1")
    (down "2")
    (left "3")))

(defun execute-move (move)
  (selenium:do-run-script
      (concatenate 'string
                   "manager.move(" (encode-move move) ");")))

(defun get-num-at (row col)
  (let* ((x (write-to-string row))
         (y (write-to-string col))
         (cmd (concatenate 'string
                           "window.manager.grid.cells[" x "][" y "].value")))
    (handler-case (parse-integer (selenium:do-get-eval cmd))
      (selenium:execution-error () 0))))
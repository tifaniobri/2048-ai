(in-package #:2048-ai)

(deftype board () '(simple-array fixnum (16)))

(defun make-board ()
  (make-array 16 :element-type 'fixnum :initial-element 0))

(defun board-ref (board x y)
  (aref board (+ x (* y 4))))

(defsetf board-ref (board x y) (val)
  `(setf (aref ,board (+ ,x (* ,y 4))) ,val))

(defun print-board (board)
  (iter (for y from 0 to 3)
        (iter (for x from 0 to 3)
              (princ (board-ref board x y))
              (finally (fresh-line)))))

(defun copy-board (board)
  (copy-seq board))

(defun get-board ()
  (let ((board (make-board)))
    (loop for x from 0 to 3
       do (loop for y from 0 to 3
             do (setf (board-ref board x y) (get-num-at x y))))
    board))

(defun same-board (board1 board2)
  (iter (for i from 0 to 15)
        (always (eql (aref board1 i) (aref board2 i)))))
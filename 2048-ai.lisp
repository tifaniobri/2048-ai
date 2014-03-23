;;;; 2048-ai.lisp

;;;; Make sure to run selenium/webserver first
(in-package #:2048-ai)

(defun won (board)
  (find 2048 board))

(defun lost (board)
  (and (same-board board (move-up board))
       (same-board board (move-down board))
       (same-board board (move-left board))
       (same-board board (move-right board))))

(defun play-game (starting-board player1-func player2-func)
  (let ((board starting-board)
        (current-player-1 T))
    (loop until (or (won board) (lost board))
       do (setf board (if current-player-1
                          (funcall player1-func board)
                          (funcall player2-func board)))
       do (setf current-player-1 (not current-player-1)))
    board))

(defun computer-player (board)
  (declare (ignore board))
  (get-board))

(defun slider-to-player (slider-func)
  (lambda (board)
    (let ((move (funcall slider-func board)))
      (execute-move move)
      (funcall (move-func-for move) board))))

(defun random-slider (board)
  (declare (ignore board))
  (random-elt '(up down left right)))

(defun random-computer-move (board)
  (let ((empty (iter (for i from 0 to 15)
                     (when (= (aref board i) 0)
                       (collect i))))
        (new (copy-board board)))
    (setf (aref new (random-elt empty)) (if (< (random 100) 10)
                                            4
                                            2))
    new))

(defun play-against-computer (player-func)
  (play-game (get-board) player-func #'computer-player))

(defun random-move-list ()
  (shuffle '(up down left right)))

(defun valid-move-p (board move)
  (not (same-board board (move board move))))

(defun valid-moves (board moves)
  (remove-if-not (curry #'valid-move-p board) moves))

(defun diff (old new n)
  (- (count n new) (count n old)))

(defun score-it (old new n)
  (* n (+ (* 2 (max 0 (diff old new (* n 2)))) (diff old new n))))

(defun score (old new)
  (flet ((s (n) (score-it old new n)))
    (+ (s 2048) (s 1024)
       (s 512) (s 256) (s 128)
       (s 64) (s 32) (s 16)
       (s 8) (s 4) (s 2))))

(defun score-maximizer (score-func board)
  (iter (for move in (valid-moves board (random-move-list)))
        (finding move maximizing (funcall score-func board (move board move)))))

(defparameter *weights*
  '#(1 0 0 0
     2 1 0 0
     3 2 1 0
     4 3 2 1))

(defun place-2-at (board pos)
  (if (= 0 (board-ref board (first pos) (rest pos)))
      (progn
        (setf (board-ref board (first pos) (rest pos)) 2)
        t)
      nil))

(defun weighted-computer (board)
  (let ((new (copy-board board)))
    (iter (for pos-list in '(((0 . 3))
                             ((0 . 2) (1 . 3))
                             ((0 . 1) (1 . 2) (2 . 3))
                             ((0 . 0) (1 . 1) (2 . 2) (3 . 3))
                             ((1 . 0) (2 . 0) (3 . 0)
                              (2 . 1) (3 . 1) 
                              (3 . 2))))
          (thereis
           (iter (for pos in (shuffle pos-list))
                 (thereis (place-2-at new pos)))))
    new))

(defun wscore (old new)
  (- (weighted-score new)
     (weighted-score old)))

(defun weighted-score (board)
  (iter (for i from 0 to 15)
           (sum (* (aref board i) (aref *weights* i)))))

(defconstant *wining-score* (- most-positive-fixnum 10))
(defconstant *lost-score* most-negative-fixnum)

(defun minimax (board computer-func ply eval-func)
  (if (= ply 0)
      (funcall eval-func board)
      (let ((moves (valid-moves board '(up down left right))))
        (cond ((won board) (+ ply *wining-score*))
              ((null moves) *lost-score*)
              (t (let ((best-move nil)
                       (best-score nil))
                   (dolist (move moves)
                     (let* ((new (move board move))
                            (score (minimax (funcall computer-func new) computer-func (- ply 1) eval-func)))
                       (when (or (null best-score)
                                 (> score best-score))
                         (setf best-score score)
                         (setf best-move move))))
                   (values best-score best-move)))))))

(defun minimax-searcher (ply score-func computer-func)
  (lambda (board)
    (multiple-value-bind (score move) (minimax board computer-func ply score-func)
      (declare (ignore score))
      move)))

(defun play ()
  (play-against-computer
   (slider-to-player (minimax-searcher 4 
                                       #'weighted-score
                                       #'weighted-computer))))
;;; MINIMAX strategy of computer player
;;; Only for educational reasons, use ALPHA-BETA instead.

(defun minimax (player board)
  "Make perfect move with minimax strategy."
  (let ((moves (legal-moves board)))
    (if (or (null moves) (has-won-p player board) (has-won-p (opponent player) board))
	(evaluate player board)
	(let ((best-val most-negative-fixnum)
              best-move)
	  (loop for move in moves
	     with val do
	       (place-piece move player board)
	       (setf val (- (minimax (opponent player) board)))
	       (place-piece move empty board)
	       (when (> val best-val)
		 (setf best-val val
                       best-move move)))
	  (values best-val best-move)))))

(defun minimax-strategy (player board)
  "Wrapper for MINIMAX."
  (multiple-value-bind (value move)
      (minimax player board)
    (declare (ignore value))
    move))

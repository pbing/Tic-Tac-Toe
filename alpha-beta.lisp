;;; ALPHA-BETA strategy of computer player

(defun alpha-beta (player board alpha beta)
  "Make perfect move with alpha-beta strategy."
  (let ((moves (legal-moves board)))
    (if (or (null moves) (has-won-p player board) (has-won-p (opponent player) board))
	(evaluate player board)
	(let (best-move)
	  (loop for move in moves
	     with val do
	       (place-piece move player board)
	       (setf val (- (alpha-beta (opponent player) board (- beta) (- alpha))))
	       (place-piece move empty board)
	       (when (> val alpha)
		 (setf alpha val
                       best-move move))
	     until (>= alpha beta))
	  (values alpha best-move)))))

(defun alpha-beta-strategy (player board)
  "Wrapper for ALPHA-BETA."
  (multiple-value-bind (value move)
      (alpha-beta player board -1 1)
    (declare (ignore value))
    move))

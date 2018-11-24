;;; Human player

(defun human (player board)
  "A human player for the game of Tic-Tac-Toe."
  (format t "~&~C to move: " (player-name-of player))
  (symbol-move (read) board))


(defun symbol-move (symbol board)
  "Convert SYMBOL \(got from READ\) to a board move."
  (let ((s (string symbol)))
    (array-row-major-index board
			   (- (char-code (char s 1)) (char-code #\1)) 
			   (- (char-code (char s 0)) (char-code #\A)))))



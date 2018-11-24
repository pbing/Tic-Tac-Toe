;;; Random strategy of computer player

(defun random-strategy (player board)
  "Make any legal move."
  (declare (ignore player))
  (let ((moves (legal-moves board)))
    (and moves (nth (random (length moves)) moves))))

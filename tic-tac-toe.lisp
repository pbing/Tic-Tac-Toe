;;; Tic-Tac-Toe
;;;
;;; For a board size of 3x3 and assuming that player X makes the first move every time there are:
;;;
;;;   - 255,168 possible games
;;;   - 131,184 games are won by X
;;;   - 77,904 games are won by O
;;;   - 46,080 games are a draw
;;;
;;; (play #'random-strategy #'random-strategy) should represent this.
;;;
;;; For playing human against computer use, e.g.
;;; (play #'human #'alpha-beta-strategy)
;;; (play #'alpha-beta-strategy #'human)
;;;
;;; Beware: (play ... :size 4) and more needs a lot of time!

(defconstant empty  0 "Empty field")
(defconstant cross  1 "Player X")
(defconstant circle 2 "Player O")

(defun initial-board (n)
  "Return a board with empty fields of size N x N."
  (make-array (list n n) :element-type `(integer ,empty ,circle) :initial-element empty))

;;; Output
(defun print-board (board)
  "Print a board."
  (let ((size (first (array-dimensions board))))
    (format t "~2&~3T")
    (loop for i below size do
	 (format t "~A " (code-char (+ (char-code #\A) i))))
    (loop for i below size do
	 (format t "~&~2D" (1+ i))
	 (loop for j below size do
	      (format t " ~A" (player-name-of (aref board i j)))))))

(defun player-name-of (player)
  "Return character of current player or of empty field."
  (char ".XO" player))

;;; Access to board
(defun place-piece (move player board)
  "Place a piece to a board location."
  (setf (row-major-aref board move) player))

(defun legal-p (move board)
  "A legal move must be into an empty field."
  (eql empty (row-major-aref board move)))

(defun legal-moves (board)
  "Return list of legal moves."
  (loop for move below (array-total-size board)
     when (legal-p move board) collect move))

;;; Play it
(defun play (strategy-1 strategy-2 &key (size 3) print-p)
  "Play the game of Tic-Tac-Toe.
STRATEGY is a member of the functions:
  HUMAN               Human player
  RANDOM-STRATEGY     Computer plays legal random move
  ALPHA-BETA-STRATEGY Computer plays with Alpha-Beta strategy
  MINIMAX-STRATEGY    Computer plays with Minimax strategy"
  (let ((board (initial-board size)))
    (when print-p (print-board board))
    (loop
       repeat (array-total-size board)           ; needed for draw
       for player = cross then (opponent player) ; X makes always the first move
       for strategy = (if (eql player cross) strategy-1 strategy-2)
       for move = (funcall strategy player board)
       do (place-piece move player board)
	 (when print-p (print-board board))
       until (or (has-won-p cross board) (has-won-p circle board)))
    (evaluate cross board)))

(defun opponent (player)
  "Return the opponent of PLAYER."
  (if (eql player cross) circle cross))

(defun has-won-p (player board)
  "A player has won if either a complete row, a complete column or a complete diagonal is finished."
  (let ((size (first (array-dimensions board))))
    (or
     (loop for r below size thereis (loop for c below size always (eql player (aref board r c))))   ; complete row?
     (loop for c below size thereis (loop for r below size always (eql player (aref board r c))))   ; complete column?
     (loop for r below size for c below size               always (eql player (aref board r c)))    ; complete 1st diagonal?
     (loop for r below size for c from (1- size) downto 0  always (eql player (aref board r c)))))) ; complete 2nd diagonal?

(defun evaluate (player board)
  "Evaluation function for BOARD."
  (cond
    ((has-won-p player            board)  1)
    ((has-won-p (opponent player) board) -1)
    (t                                    0)))

(defun tournament (strategy-1 strategy-2 &optional (matches 1))
  "Comparing strategies between player and opponent."
  (loop repeat matches
     for score = (play strategy-1 strategy-2)
     when (= score 1) sum 1 into player-score
     when (= score -1) sum 1 into opponent-score
     when (= score 0) sum 1 into draws
     finally (return (values player-score opponent-score draws))))

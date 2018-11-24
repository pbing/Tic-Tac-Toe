;;; -*- Lisp -*-
;;;
;;; Tic-Tac-Toe
;;;
;;; Some parts are unashamedly stolen from:
;;; Norvig, Peter. Paradigms of artificial intelligence programming: case studies in Common Lisp, pp. 596,
;;; Morgan Kaufmann Publishers, Inc., 1992.

(defsystem "tic-tac-toe"
  :components ((:file "tic-tac-toe")
	       (:file "human")
	       (:file "random-strategy")
	       (:file "minimax")
	       (:file "alpha-beta")))

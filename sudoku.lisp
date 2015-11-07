;;; CONSTANTS VARIABLES
(setf *header* "   | A B C | D E F | G H I |")
(setf *stars* "****************************")

;; MAIN PROC
(defun sudoku (board)  
  (print-board board))

;; DEBUG MAIN PROC
(defun sudoku-t ()
  (defparameter *grid* (make-array '(9 9) :initial-contents '((1 0 0 0 0 4 0 0 5)
     (0 0 0 9 5 0 0 8 0)
     (0 0 0 0 0 3 0 9 0)
     (0 0 5 0 0 2 0 0 4)
     (0 0 1 0 6 0 7 0 0)
     (7 0 0 3 0 0 2 0 0)
     (0 6 0 5 0 0 0 0 0)
     (0 8 0 0 1 6 0 0 0)
     (5 0 0 2 0 0 0 0 7))))
    *grid*)

(defun print-board (board)
  ;; HEADER
  (princ *header*)
  (print-newline)
  (princ *stars*)
  (print-newline)
  ;; GAME
  (loop for x from 0 to 8
     do
       (when (and (= (mod x 3) 0) (not (= x 0)))
	     (princ *stars*)
	     (print-newline))
       (print-line board x)
       (print-newline))
  ;; FOOTER
  (princ *stars*) (print-newline))

(defun print-line (board num)
  (format t " ~D | " num)
  (loop for x from 0 to 8
     do
       (if (and (= (mod x 3) 0) (not (= x 0)))
	   (format t "~A " "|"))
       (format t "~A " (get-value-at board num x)))
  (format t "~A " "|"))

(defun print-newline () 
  (format t "~C" #\linefeed))

(defun get-value-at (board x y)
  (aref board x y))

(defun get-value (board x y)
  (if (and (> (char-code y) 96) (< (char-code y) 106) (> x -1) (< x 11))
      (aref board x (mod (char-code y) 97))
      "Case inexistante"))

(defun change-value (board x y z)
    (if (and (> (char-code y) 96) (< (char-code y) 106) (> x -1) (< x 11) (> z -1) (< z 11))
	"Valeurs invalides"
	(setf (aref board x (mod (char-code y) 97))  z))
	(print-board board))


;;; permet de valider si la grille est correctement remplit
(defun solver (board)
  (let ((sommeL 0)
	(sommeC 0)
	(solved T))
    (loop for i from 0 to 8
       do
	 (loop for j from 0 to 8
	    do
	      (setq sommeL (+ sommeL (aref board j i)))
	      (setq sommeC (+ sommeC (aref board i j))))

	 (if (or (not (= sommeL 45)) (not (= sommeC 45)))
	     (setq solved nil)))
    (if (not (eq solved nil))
	(setq solved T))))
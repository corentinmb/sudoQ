;;;; sudoku.lisp --- Sudoku Puzzle
;; Université de Bordeaux
;;
;; Author: Corentin Davidenko <corentin.davidenko@etu.u-bordeaux.fr>
;; Author: Corentin Moreau <corentin.moreau@etu.u-bordeaux.fr>
;; Author: Antoine Rivalier <antoine.rivalier@etu.u-bordeaux.fr>
;; GIT: https://github.com/corentinmb/sudoQ/
;; Created: 31-10-2015


;;; Constants variables defined for the program
(defparameter header "   | A B C | D E F | G H I |")
(defparameter stars "****************************")
(defparameter actual_board (make-array '(9 9) :initial-element 0))
(defparameter solved nil)
(defparameter currentX -1)
(defparameter currentY ".")
(defparameter currentValue -1)
(defparameter grid-test (make-array '(9 9) :initial-contents '((1 0 0 0 0 4 0 0 5)
							       (0 0 0 9 5 0 0 8 0)
							       (0 0 0 0 0 3 0 9 0)
							       (0 0 5 0 0 2 0 0 4)
							       (0 0 1 0 6 0 7 0 0)
							       (7 0 0 3 0 0 2 0 0)
							       (0 6 0 5 0 0 0 0 0)
							       (0 8 0 0 1 6 0 0 0)
							       (5 0 0 2 0 0 0 0 7))))

;;; SUDOKU function
;;; This is the main function to run the game
(defun sudoku (board)
  (defparameter actual_board board)
  (loop while (not solved)
     do
       (print-board)
       (play)))

;;; PLAY function
;;; Function called to play with the user
(defun play()
  (setq currentX -1)
  (setq currentY ".")
  (setq currentValue -1)
  (loop while (not (is-correct currentX currentY currentValue))
     do
       (print-newline)
       (princ "C L ? ")
       (setq currentY (read))
       (setq currentX (read))
       (princ "Value ? ")
       (setq currentValue (read))
       (if (not (is-correct currentX currentY currentValue))
	   (princ "[ERREUR] Coordonnees et/ou Valeur incorrecte !")))
  (change-value currentX currentY currentValue))
  
;;; PRINT-BOARD function
;;; Print actual_grid on the screen
(defun print-board ()
  ;; HEADER
  (princ header)
  (print-newline)
  (princ stars)
  (print-newline)
  ;; GAME
  (loop for x from 0 to 8
     do
       (when (and (= (mod x 3) 0) (not (= x 0)))
	     (princ stars)
	     (print-newline))
       (print-line x)
       (print-newline))
  ;; FOOTER
  (princ stars)
  (print-newline)
  T)

;;; PRINT-LINE function
;;; Print the num line on the screen
(defun print-line (num)
  (format t " ~D | " (1+ num))
  (loop for x from 0 to 8
     do
       (if (and (= (mod x 3) 0) (not (= x 0)))
	   (format t "~A " "|"))
       (format t "~A " (get-value-at num x)))
  (format t "~A " "|"))

;;; PRINT-NEWLINE function
;;; return a line break
(defun print-newline () 
  (format t "~C" #\linefeed))

;;; GET-VALUE-AT function
;;; return the value of the cell x;y
(defun get-value-at (x y)
  (aref actual_board x y))

;;; IS-CORRECT function
;;; Check if the input is well-formed
(defun is-correct(x y &optional z)
  (setq y (coerce y 'character))
  (if (and (> x 0)
	   (< x 10)
	   (or
	    (and
	     (> (char-code y) 64)
	     (< (char-code y) 74))
	    (and
	     (> (char-code y) 96)
	     (< (char-code y) 106))))
      (if (or
	   (not z)
	   (and
	    (> z 0)
	    (< z 10)))
	  T
	  NIL)
      NIL))
 
;;; CHANGE-VALUE function
;;; Set a new value z to the cell in x;y
(defun change-value (x y z)
  (setq y (coerce y 'character))
  (if (and (> x 0)
	   (< x 10)
	   (> z 0)
	   (< z 10))
      (cond ((and (> (char-code y) 64)
		  (< (char-code y) 74))
	     (setf (aref actual_board (1- x) (mod (char-code y) 65) ) z))
	    ((and (> (char-code y) 96)
		  (< (char-code y) 106))
	     (setf (aref actual_board (1- x) (mod (char-code y) 97) ) z)))))


;;; IS-SOLVED function
;;; Check the state of actual_board (solved or not...)
(defun is-solved (board)
  (let ((solved T)
	(tabC (make-array 9))
	(tabL (make-array 9)))
    (loop for i from 0 to 8
       do
	 (loop for j from 0 to 8
	    do
	      (if (null (find (aref board j i) tabC))
		  (setf (aref tabC j) (aref board j i))
		  (setq solved nil))
	      (if (null (find (aref board i j) tabL))
		  (setf (aref tabL j) (aref board i j))
		  (setq solved nil)))
	 (setq tabC (make-array 9))
	 (setq tabL (make-array 9)))
	solved))

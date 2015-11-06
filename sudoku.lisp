;;; CONSTANTS VARIABLES
(defvar *header* "  | A B C | D E F | G H I |")
(defvar *stars* "****************************")


(defun sudoku (board)  
  (print-board board))

(defun print-board (board)
  (princ *header*)
  (print-newline)
  (princ *stars*)
  (print-newline)
  ;; 2EME ETAGE
  (print-line board 0)
  (print-newline)
  (princ *stars*)
  (print-newline)
  ;; 1ER ETAGE
  (print-line board 1)
  (print-newline)
  (princ *stars*)
  (print-newline)
  ;; RDC
  (print-line board 2)
  (print-newline)
  (princ *stars*)
  (print-newline))

(defun print-line (board num)
  (format t " ~D | " num)
  (loop for x from 0 to 8
     do
       (format t "~A " (get-value-at board num x))
       (if (and (= (mod x 3) 0) (not (= x 0)))
	   (format t "~A " "|"))))

(defun print-newline () 
  (format t "~C" #\linefeed))

(defun get-value-at (board x y)
  (aref board x y))

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
  (print-board *grid*))

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
  (princ *stars*)
  (print-newline))

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

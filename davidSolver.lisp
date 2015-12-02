;;;IA by Davidenko

(let ((sudoku-size 9))
  ;;(square-size 3))
  
(defun values-of-colon (grid x)
  (let ((l '()))
  (dotimes (i sudoku-size)
    (when (not (zerop (aref grid i x)))
      (push (aref grid i x) l)))
  l))

(defun values-of-line (grid y)
  (let ((l '()))
  (dotimes (i sudoku-size)
    (when (not (zerop (aref grid y i)))
      (push (aref grid y i) l)))
  l))

(defun values-of-square (grid x y)
  (let* ((lmin (* 3 (floor y 3)))
	 (lmax (+ 3 lmin))
	 (cmin (* 3 (floor x 3)))
	 (cmax (+ 3 cmin))
	 (l '()))
    (do ((c cmin (1+ c))) ((= c cmax))
      (do ((r lmin (1+ r))) ((= r lmax))
	(when (> (aref grid r c) 0)
	  (push (aref grid r c) l))))
    l))


(defun missings (presents)
  (let ((missings '()))
    (do ((i 1 (1+ i))) ((= i 9))
      (unless (member i presents) (push i missings)))
    missings))
    
)


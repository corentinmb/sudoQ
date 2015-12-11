;;;IA by Davidenko

  
(defun values-of-colon (grid x)
  (let ((l '()))
  (dotimes (i *sudoku-size*)
    (when (not (zerop (aref grid i x)))
      (push (aref grid i x) l)))
  l))

(defun values-of-line (grid y)
  (let ((l '()))
  (dotimes (i *sudoku-size*)
    (when (not (zerop (aref grid y i)))
      (push (aref grid y i) l)))
  l))

(defun values-of-square (grid x y)
  (let* ((lmin (* *sudoku-square-size* (floor x *sudoku-square-size*)))
	 (lmax (+ *sudoku-square-size* lmin))
	 (cmin (* *sudoku-square-size* (floor y *sudoku-square-size*)))
	 (cmax (+ *sudoku-square-size* cmin))
	 (l '()))
    (do ((c cmin (1+ c))) ((= c cmax))
      (do ((r lmin (1+ r))) ((= r lmax))
	(when (> (aref grid r c) 0)
	  (push (aref grid r c) l))))
    l))


(defun missings (presents)
  (let ((missings '()))
    (do ((i 1 (1+ i))) ((= i (1+ *sudoku-size*)))
      (unless (member i presents) (push i missings)))
    missings))

(defun missing-at (x y)
  (missings (nconc (values-of-line *grid* x) (values-of-colon *grid* y) (values-of-square *grid* x y))))
  
(defparameter *grid* (make-array '(9 9) :initial-element 0)) 
(defparameter *list-of-plays* '())
(defparameter *cell-list* '())
(defparameter *sudoku-size* 9)
(defparameter *sudoku-square-size* 3)


(defun init-standalone (grid)
  (setf *grid* (copy-array grid))
  (setf *sudoku-size* (car (array-dimensions *grid*)))
  (setf *sudoku-square-size* (truncate (sqrt *sudoku-size*)))
  (solver (classement-des-cases )))

(defun classement-des-cases ()
  (let ((cells '() ))
    (dotimes (i *sudoku-size*)
      (dotimes (j *sudoku-size*)
	(when (= 0 (aref *grid* i j))
	  (push (cons (cons i j) (count-missings i j)) cells))))
    (sort cells #'< :key #'cdr)
    ))

(defun count-missings (x y)
  (length (missing-at x y)))

(defun solver (classed-cells)
  (if (null classed-cells)
      T
      (let* ((cell (car (car classed-cells)))
	     (x (car cell))
	     (y (cdr cell))
	     (boolean nil)
	     (possibilities (missing-at x y)))
	(loop for i in possibilities do
	     (setf (aref *grid* x y) i)
	     (when (solver (cdr classed-cells))
		   (push (list x y i) *list-of-plays*)
		   (setf boolean T)))
	(if boolean
	    T
	    (progn (setf (aref *grid* x y) 0) nil))
	)))
	
		 
		 
	


(defun main-standalone ())
  

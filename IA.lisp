  (defun IArandom(grid)
  (let ((tabNbOc (make-array '(3 3) :initial-contents '((0 0 0) (0 0 0) (0 0 0))))
	(tabC (make-array 9))
	(tabL (make-array 9)))

  ;;; premier balayage pour tout remplir aléatoirement
  (loop for i from 0 to 8 do
      (loop for j from 0 to 8 do
	   (setf (aref grid i j) (random 10))))

    (loop for i from 0 to 8
       do
	 (loop for j from 0 to 8
	    do
	      ;;; condition sur les colonnes
	      (if (null (find (aref board j i) tabC))
		  (setf (aref tabC j) (aref board j i))

		  (if (and (> j -1) (< j 3))
		      (setf (aref tabNbOc 0 0) (1+ (aref tabNbOc 0 0)) (aref tabNbOc 1 0) (1+ (aref tabNbOc 1 0)) (aref tabNbOc 2 0) (1+ (aref tabNbOc 2 0)))
		      (if (and (> j 2) (< j 6))
			  (setf (aref tabNbOc 0 1) (1+ (aref tabNbOc 0 1)) (aref tabNbOc 1 1) (1+ (aref tabNbOc 1 1)) (aref tabNbOc 2 1) (1+ (aref tabNbOc 2 1)))
			  (setf (aref tabNbOc 0 2) (1+ (aref tabNbOc 0 2)) (aref tabNbOc 1 2) (1+ (aref tabNbOc 1 2)) (aref tabNbOc 2 2) (1+ (aref tabNbOc 2 2)))))
	      
	      ;;; condition sur les lignes
	      (if (null (find (aref board i j) tabL))
		  (setf (aref tabL j) (aref board i j))

		  (if (and (> i -1) (< i 3))
		     (setf (aref tabNbOc 0 0) (1+ (aref tabNbOc 0 0)) (aref tabNbOc 0 1) (1+ (aref tabNbOc 0 1)) (aref tabNbOc 0 2) (1+ (aref tabNbOc 0 2)))
		      (if (and (> i 2) (< i 6))
			  (setf (aref tabNbOc 1 0) (1+ (aref tabNbOc 1 0)) (aref tabNbOc 1 1) (1+ (aref tabNbOc 1 1)) (aref tabNbOc 1 2) (1+ (aref tabNbOc 1 2)))
			  (setf (aref tabNbOc 2 0) (1+ (aref tabNbOc 2 0)) (aref tabNbOc 2 1) (1+ (aref tabNbOc 2 1)) (aref tabNbOc 2 2) (1+ (aref tabNbOc 2 2)))))))

	 (setq tabC (make-array 9))
	 (setq tabL (make-array 9)))))
 ;;; IA aléatoire

(defun IArandom(grid)
  (let ((tabNbOc (make-array '(3 3) :initial-contents '((0 0 0) (0 0 0) (0 0 0))))
        (tabC (make-array 9))
        (tabL (make-array 9))
        (carreMax '(0 0)))
 
    (loop for i from 0 to 8
       do
         (loop for j from 0 to 8
            do
	       ;;; remplissage aléatoire de la matrice
	      (if (not (= (aref grid i j) 0))
		  (setf (aref grid i j) (random 10)))
	      
              ;;; condition sur les colonnes
              (if (null (find (aref grid j i) tabC))
                  (setf (aref tabC j) (aref grid j i))
 
                  (if (and (> j -1) (< j 3))
                      (setf (aref tabNbOc 0 0) (1+ (aref tabNbOc 0 0)) (aref tabNbOc 1 0) (1+ (aref tabNbOc 1 0)) (aref tabNbOc 2 0) (1+ (aref tabNbOc 2 0)))
                      (if (and (> j 2) (< j 6))
                          (setf (aref tabNbOc 0 1) (1+ (aref tabNbOc 0 1)) (aref tabNbOc 1 1) (1+ (aref tabNbOc 1 1)) (aref tabNbOc 2 1) (1+ (aref tabNbOc 2 1)))
                          (setf (aref tabNbOc 0 2) (1+ (aref tabNbOc 0 2)) (aref tabNbOc 1 2) (1+ (aref tabNbOc 1 2)) (aref tabNbOc 2 2) (1+ (aref tabNbOc 2 2))))))
             
              ;;; condition sur les lignes
              (if (null (find (aref grid i j) tabL))
                  (setf (aref tabL j) (aref grid i j))
 
                  (if (and (> i -1) (< i 3))
                     (setf (aref tabNbOc 0 0) (1+ (aref tabNbOc 0 0)) (aref tabNbOc 0 1) (1+ (aref tabNbOc 0 1)) (aref tabNbOc 0 2) (1+ (aref tabNbOc 0 2)))
                      (if (and (> i 2) (< i 6))
                          (setf (aref tabNbOc 1 0) (1+ (aref tabNbOc 1 0)) (aref tabNbOc 1 1) (1+ (aref tabNbOc 1 1)) (aref tabNbOc 1 2) (1+ (aref tabNbOc 1 2)))
                          (setf (aref tabNbOc 2 0) (1+ (aref tabNbOc 2 0)) (aref tabNbOc 2 1) (1+ (aref tabNbOc 2 1)) (aref tabNbOc 2 2) (1+ (aref tabNbOc 2 2))))))))
 
         (setf tabC (make-array 9))
         (setf tabL (make-array 9))

	 ;;; faudrait faire un while(!issolved)
         (let ((nbOc 0))
           (loop for i from 0 to 8
              do
                (loop for j from 0 to 8
                     do
                     (if (> (aref tabNbOc i j) nbOc)
                         (setf nbOc (aref tabNbOc i j) carreMax '(i j))))))

	 (aleaCarre (car carreMax) (car (cdr carreMax)) grid)

	 
	 ))

;;; mélange aléatoirement les valeurs d'un carré

(defun aleaCarre(i j grid)
  (let ((valDebI 0)
	(valDebJ 0))
    
    (if (= i 1)
	(setf valDebI 3)
	(if (= i 2)
	    (setf valDebI 6)))

    (if (= j 1)
	(setf valDebJ 3)
	(if (= j 2)
	    (setf valDebJ 6)))

    (loop for k from valDebI to (+ valDebI 2)
       do
	 (loop for l from valDebJ to (+ valDebJ 2)
	    do
	      (setf (aref grid k l) (random 10))))))

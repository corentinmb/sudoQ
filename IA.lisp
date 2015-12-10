;;; IA aléatoire

(defun IArandom(grid)
  (let ((tabNbOc (make-array '(3 3) :initial-contents '((0 0 0) (0 0 0) (0 0 0))))
        (carreMax '(0 0)))
    
    (loop for i from 0 to 2
       do
         (loop for j from 0 to 2
            do
	      ;;; remplissage aléatoire de la matrice
	      (aleaCarre i j grid)))                    

	 ;;; boucle while permettant de reset les carrés
	 (loop do

	      (let ((nbOc 0))
		(findOcur grid tabNbOc)
		(loop for i from 0 to 2
		   do
		     (loop for j from 0 to 2
			do
			  (if (> (aref tabNbOc i j) nbOc)
			      (setf nbOc (aref tabNbOc i j) carreMax (list i j)))))
		(aleaCarre (car carreMax) (car (cdr carreMax)) grid))

	      (print-board)
	      (print tabNbOc)
	      (setf tabNbOc (make-array '(3 3)))
	      
	    while(eq (is-solved grid) nil))

	 
	 ))

;; trouver des occurences

(defun findOcur(grid tabOc)
  (let ((tabC (make-array 9))
        (tabL (make-array 9)))
    
    (loop for i from 0 to 8
       do
	 (loop for j from 0 to 8
	    do
	    ;;; condition sur les colonnes
              (if (null (find (aref grid j i) tabC))
                  (setf (aref tabC j) (aref grid j i))
 
                  (if (and (> i -1) (< i 3))
                      (setf (aref tabOc 0 0) (1+ (aref tabOc 0 0)) (aref tabOc 1 0) (1+ (aref tabOc 1 0)) (aref tabOc 2 0) (1+ (aref tabOc 2 0)))
                      (if (and (> i 2) (< i 6))
                          (setf (aref tabOc 0 1) (1+ (aref tabOc 0 1)) (aref tabOc 1 1) (1+ (aref tabOc 1 1)) (aref tabOc 2 1) (1+ (aref tabOc 2 1)))
                          (setf (aref tabOc 0 2) (1+ (aref tabOc 0 2)) (aref tabOc 1 2) (1+ (aref tabOc 1 2)) (aref tabOc 2 2) (1+ (aref tabOc 2 2))))))
             
              ;;; condition sur les lignes
              (if (null (find (aref grid i j) tabL))
                  (setf (aref tabL j) (aref grid i j))
 
                  (if (and (> i -1) (< i 3))
		      (setf (aref tabOc 0 0) (1+ (aref tabOc 0 0)) (aref tabOc 0 1) (1+ (aref tabOc 0 1)) (aref tabOc 0 2) (1+ (aref tabOc 0 2)))
                      (if (and (> i 2) (< i 6))
                          (setf (aref tabOc 1 0) (1+ (aref tabOc 1 0)) (aref tabOc 1 1) (1+ (aref tabOc 1 1)) (aref tabOc 1 2) (1+ (aref tabOc 1 2)))
                          (setf (aref tabOc 2 0) (1+ (aref tabOc 2 0)) (aref tabOc 2 1) (1+ (aref tabOc 2 1)) (aref tabOc 2 2) (1+ (aref tabOc 2 2)))))))
	 (setf tabC (make-array 9) tabL (make-array 9)))))
	    
;;; mélange aléatoirement les valeurs d'un carré

(defun aleaCarre(i j grid)
  (let ((valDebI 0)
	(valDebJ 0)
	(listeVal '()))
    
    (if (= i 1)
	(setf valDebI 3)
	(if (= i 2)
	    (setf valDebI 6)))

    (if (= j 1)
	(setf valDebJ 3)
	(if (= j 2)
	    (setf valDebJ 6)))

    (loop for k from valDebI to (+ valDebI 2)
       do ;;; mettre les valeurs non modifiables dans la liste des le début
	 (loop for l from valDebJ to (+ valDebJ 2)
	    do
	      (if (= (aref nModifBoard k l) 0)
		  (progn (let ((rand 0))
		    (loop do			 
			 (setf rand (1+ (random 9)))
		       while(> (count rand listeVal) 0))
		    (setf (aref grid k l) rand listeVal (append listeVal (list rand)))))
		  (setf listeVal (append listeVal (list (aref nModifBoard k l)))))))(print listeVal)))

;;; fonction générique pour copier un tableau

(defun copy-array (array &key
			   (element-type (array-element-type array))
			   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
			   (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

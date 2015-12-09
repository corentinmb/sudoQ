;;; IA aléatoire

(defun IArandom(grid)
  (let ((tabNbOc (make-array '(3 3) :initial-contents '((0 0 0) (0 0 0) (0 0 0))))
        (carreMax '(0 0)))
 
    (loop for i from 0 to 8
       do
         (loop for j from 0 to 8
            do
	      ;;; remplissage aléatoire de la matrice
	      (if (= (aref grid i j) 0)
		  (setf (aref grid i j) (1+ (random 9))))))                    

	 ;;; boucle while permettant de reset les carrés
	 (loop do

	      (let ((nbOc 0))
		(findOcur grid tabNbOc)
		(aleaCarre (car carreMax) (car (cdr carreMax)) grid)
		(loop for i from 0 to 2
		   do
		     (loop for j from 0 to 2
			do
			  (if (> (aref tabNbOc i j) nbOc)
			      (setf nbOc (aref tabNbOc i j) carreMax (list i j))))))

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
       do
	 (loop for l from valDebJ to (+ valDebJ 2)
	    do
	      (if (and (= (aref nModifBoard k l) 0))
		  (let ((rand 0))
		    (loop do
			 ;;;(setf (aref grid k l) (1+ (random 9)) listeVal (append listeVal (list (aref grid k l))))
			 (setf rand (1+ (random 9)))
		       while(> (count rand listeVal) 0))
		    (setf (aref grid k l) rand listeVal (append listeVal (list rand)))))))))

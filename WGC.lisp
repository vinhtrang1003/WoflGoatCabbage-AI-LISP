;;; Vinh Trang
;;; IA3476
;;; CS461
;;; 09/24/2019
;;; Using depth first
;;; search to search all
;;; state spaces in the tree

; let person wolf goat and cabbage be p w g c
; let the both river bank be a and b where
; the person need to take w g c from bank a to bank b

(defun edit-state (p w g c) (list p w g c))

(defun farmer-bank(state)
	(nth 0 state))

(defun wolf-bank(state)
	(nth 1 state))

(defun goat-bank (state)
	(nth 2 state))

(defun cabbage-bank(state)
	(nth 3 state))

	
; function to change bank for pwgc	
(defun changebank (bank)
	(cond ((equal bank 'a) 'b)
		((equal bank 'b) 'a)))
	
; person can cross river alone

(defun farmer-with-self (state)
	(islegal (edit-state (changebank (farmer-bank state))
								(wolf-bank state)
								(goat-bank state)
								(cabbage-bank state))))
		
; function to take wolf goat and cabbage	

(defun farmer-with-wolf (state)
   (cond ((equal (farmer-bank state) (wolf-bank state))
			(islegal (edit-state 
				(changebank (farmer-bank state))
				(changebank (wolf-bank state))
				(goat-bank state)
				(cabbage-bank state))))
		(t nil)))
				
(defun farmer-with-goat (state)
   (cond ((equal (farmer-bank state) (goat-bank state))
			(islegal (edit-state (changebank (farmer-bank state))
				(wolf-bank state)
				(changebank (goat-bank state))
				(cabbage-bank state)))) 
		(t nil)))
				
(defun farmer-with-cabbage (state)
   (cond ((equal (farmer-bank state) (cabbage-bank state))
			(islegal (edit-state (changebank (farmer-bank state))
				(wolf-bank state)
				(goat-bank state)
				(changebank (cabbage-bank state)))))   
		(t nil)))
			
;we have been using islegal function now define what is islegal

(defun islegal (state)
	(cond ((and (equal (goat-bank state) (wolf-bank state))
			(not (equal (farmer-bank state) (wolf-bank state)))) 
				nil) ; can't have wolf and goat together without person
			((and (equal (goat-bank state) (cabbage-bank state))
			(not (equal (farmer-bank state) (goat-bank state)))) 
				nil) ; can't have goat and cabbage together without person
		(t state)))
	
(defun pwgc (state finish)(depthsearch state finish nil))	
		
(defun depthsearch (state finish listtaken)
   (cond ((null state) nil)
	 ((equal state finish) (reverse (cons state listtaken)))
	 ((not (member state listtaken :test #'equal))
	      (or (depthsearch (farmer-with-self state) finish (cons state listtaken))
	          (depthsearch (farmer-with-wolf state) finish (cons state listtaken))
	          (depthsearch (farmer-with-goat state) finish (cons state listtaken))
	          (depthsearch (farmer-with-cabbage state) finish (cons state listtaken))))))

			  
(setq finishstate '(b b b b))

(setq biginstate '(a a a a))

(format t "The order from begin state to finish state of (Person Wolf Goat Cabbage) from A to B is:")

(setq result (pwgc biginstate finishstate))

(setq formatt'((P W G C) (P W G C) (P W G C) (P W G C) (P W G C) (P W G C) (P W G C) (P W G C) ))
(format t "~% ~s"formatt)
(print " --------- --------- --------- --------- --------- --------- --------- ---------   ")
(format t "~% ~s"result)










		
		

(defun c:dodl ()
  (addODB "DODL")
)

(defun c:dodr ()
  (addODB "DODR")
)

(defun c:uodl ()
  (addODB "UODL")
)

(defun c:uodr ()
  (addODB "UODR")
)

(defun addODB (blkName)
  (princ
    "\n****************************************************
     \n Developed by Peyman Bayat
     \n p2436b@gmail.com
     \n****************************************************\n"
  )
  (setvar "DIMZIN" 3)
  (setq oldAttdia (getvar "ATTDIA"))
  (setq oldAttreq (getvar "ATTREQ"))
  (setvar "ATTDIA" 0)
  (setvar "ATTREQ" 0)
  (setq	insertionPt
	 (getpoint "\nSpecify insertion point: ")
  )

  (setq di (/ (cadr insertionPt) 100))
  (setq attrValue (formatNumber di))

  (command "INSERT" blkName insertionPt 1 1 0)
  (setq blkRef (entlast))
  (if (and blkRef (eq (cdr (assoc 0 (entget blkRef))) "INSERT"))
    (progn
      (setq ent (entnext blkRef))
      (while ent
	(if (eq (cdr (assoc 0 (entget ent))) "ATTRIB")
	  (progn
	    (entmod (subst (cons 1 attrValue)
			   (assoc 1 (entget ent))
			   (entget ent)
		    )
	    )
	    (entupd ent)
	  )
	)
	(setq ent (entnext ent))
      )
      (princ "\nBlock with attribute inserted and updated.")
    )
    (princ "\nFailed to insert the block or find the attribute."
    )
  )
  (setvar "ATTDIA" oldAttdia)
  (setvar "ATTREQ" oldAttreq)
  (princ)
)



(defun formatNumber (num)
  (setq tolerance 1e-8)
  (cond
    ((and (>= num (- tolerance)) (<= num tolerance))
     (strcat "" "±0.00")
    )
    ((>= num 1)
     (strcat "+" (rtos num 2 2))
    )
    ((< num 0)
     (rtos num 2 2)
    )
    ((and (> num 0) (< num 1))
     (strcat "+" (rtos num 2 2))
    )
  )
)
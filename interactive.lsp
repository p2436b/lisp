;;  TextAlignWithObject.lsp
;;  CAB   02/19/2007
;;
;;  Add text to DWG at angle of selected object

(defun c:tao () (c:TextAlignWithObject)) ; shortcut

(defun c:TextAlignWithObject (/ tmp ang p@pt parA parB pt start txtht FixTextAngle 
                              addtext
                             ) 
  (vl-load-com)
  ;;  Returns a text angle in radians, flops text at >90 and <270
  (defun FixTextAngle (ang) 
    (if (and (> ang (* 0.5 pi)) (< ang (* 1.5 pi))) 
      (+ ang pi)
      ang
    )
  )


  ;;  Create a text object
  (defun addtext (ipt hgt text ang lay / txtObj) 
    (setq txtObj (vla-addtext 
                   (if (= (getvar 'cvport) 1) 
                     (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object)))
                     (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                   )
                   text
                   (vlax-3d-point ipt)
                   hgt
                 )
    )
    (vla-put-layer txtObj lay)
    (vla-put-rotation txtObj ang)
    (vla-put-alignment txtObj acalignmentbottomleft)
    (vla-put-textalignmentpoint txtobj (vlax-3d-point ipt))
  )

  ;;  -=< START HERE  >=-

  ;;  Get text string to insert
  (or txtstr (setq txtstr "Default Text"))
  (if 
    (/= (setq tmp (getstring t (strcat "\nEnter text string: < " txtstr " > "))) 
        ""
    )
    (setq txtstr tmp)
  )

  ;;  Get object to align text & insert point
  ;;  Object must have curve data
  (if 
    (and (setq ent (entsel "\nSelect point on object to label.")) 
         (not 
           (vl-catch-all-error-p 
             (setq pt (vl-catch-all-apply 
                        'vlax-curve-getClosestPointTo
                        (list (car ent) (cadr ent))
                      )
             )
           )
         )
    )
    (progn 
      (setq ent  (car ent)
            p@pt (vlax-curve-getParamAtPoint ent pt)
            parA (max 0.0 (- p@pt 0.05))
            parB (min (+ p@pt 0.05) (vlax-curve-getEndParam ent))
            ang  (angle (vlax-curve-getPointAtParam ent parA) 
                        (vlax-curve-getPointAtParam ent ParB)
                 ) ; aprox angle of curve at pick point
            ang  (FixTextAngle ang)
      )
      ;;  Text height by style or current Text Size
      (if (zerop (setq txtht (getvar 'textsize))) 
        (setq txtht (getvar "TextSize"))
      )
      (addtext pt 2 txtstr ang (getvar "clayer")) ; ins hgt text ang
    )
    (prompt "\n**  Missed or no curve data for object.")
  )
  (princ)
)
(prompt "\nTextAlignWithObject.lsp loaded enter TAO to run.")
(princ)

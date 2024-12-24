(defun kit ()
  (vl-load-com)
  (setq modelSpace vla-get-modelspace
        ((vla-get-activedocument (vlax-get-acad-object)))
  )
  
  (setq (grread ()))
  (while
    (setq pt (getpoint "\nEnter text location: "))
    (progn
      (setq text (getstring "\Enter text: "))
      (setq textObj (vla-addtext modelSpace text (vlax-3d-point (car pt) (cadr pt) 0.0) 5))
    )
  )
  (princ)
)
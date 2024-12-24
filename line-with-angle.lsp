; (defun lwt() (LineWithText))

(defun c:LineWithText () 
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace doc))

  (setq pt1 (getpoint "Enter first point: "))
  (setq pt2 (getpoint "Enter second point: "))
  (setq pt3 (getpoint "Enter third point: "))

  (setq points (list (car pt1) 
                     (cadr pt1)
                     0
                     (car pt2)
                     (cadr pt2)
                     0
                     (car pt3)
                     (cadr pt3)
                     0
               )
  )

  (setq ang (FixTextAngle (angle pt2 pt3)))

  (setq txtObj (vla-addtext modelSpace 
                            "123.00"
                            (vlax-3d-point 
                              (list (car pt3) 
                                    (cadr pt3)
                                    0
                              )
                            )
                            2
               )
  )

  (setq ta acalignmentbottomleft)

  (if (and pt2 pt3) 
    (progn 
      (setq x1 (car pt2)
            y1 (cadr pt2)
      ) ;; First point coordinates
      (setq x2 (car pt3)
            y2 (cadr pt3)
      ) ;; Second point coordinates
      ;; Determine direction
      (cond 
        ;; Horizontal line
        ((= y1 y2)
         (if (> x2 x1) 
           (setq ta acalignmentbottomright)
         )
        )
        ;; Vertical line
        ((= x1 x2)
         (if (> y2 y1) 
           (setq ta acalignmentbottomright)
           (setq ta acalignmentbottomright)
         )
        )
        ;; Diagonal lines
        ((and (/= x1 x2) (/= y1 y2))
         (cond 
           ((and (> x2 x1) (> y2 y1))
              (setq ta acalignmentbottomright)
           )
           ((and (> x2 x1) (< y2 y1))
            (setq ta acalignmentbottomright)
           )
         )
        )
      )
    )
    (princ "\nInvalid points selected.")
  )


  (vla-put-rotation txtObj ang)
  (vla-put-alignment txtObj ta)
  (vla-put-textalignmentpoint txtobj 
                              (vlax-3d-point 
                                (list (car pt3) 
                                      (cadr pt3)
                                      0
                                )
                              )
  )



  (setq vertices (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))))
  (vlax-safearray-fill vertices points)
  (setq polyline (vla-addpolyline modelSpace vertices))
  (princ "\nPolyline added to ModelSpace.")
  (princ)
)


(defun FixTextAngle (ang) 
  (if (and (> ang (* 0.5 pi)) (< ang (* 1.5 pi))) 
    (+ ang pi)
    ang
  )
)
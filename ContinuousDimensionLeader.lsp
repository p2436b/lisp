;; Set Text Heigh
(defun c:cdlth () 
  (setq texth (getenv "cdl_texth"))
  (if texth 
    (setq th (getreal 
               (strcat "\nEnter text heigh <" texth ">:")
             )
    )
    (setq th (getreal "\nEnter text heigh: "))
  )

  (if th 
    (setenv "cdl_texth" (rtos th 2 1))
  )
)

(defun DrawPolylineWithText (ms pt1 text-label) 
  (setq pt2 (getpoint pt1 "\nEnter Leader second point: "))
  (setq pt3 (getpoint pt2 
                      "\nEnter Leader third point (for second line): "
            )
  )


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



  (setq vertices (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))))
  (vlax-safearray-fill vertices points)
  (vla-addpolyline modelSpace vertices)


  (setq th (atof (getenv "cdl_texth")))
  (setq text-angle (FixTextAngle (angle pt2 pt3)))
  (setq txtObj (vla-addtext modelSpace 
                            text-label
                            (vlax-3d-point 
                              (list (car pt3) 
                                    (cadr pt3)
                                    0
                              )
                            )
                            th
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


  (vla-put-rotation txtObj text-angle)
  (vla-put-alignment txtObj ta)
  (vla-put-textalignmentpoint txtobj 
                              (vlax-3d-point 
                                (list (car pt3) 
                                      (cadr pt3)
                                      0
                                )
                              )
  )
)


;; ContinuousDimensionLeader 1.3
(defun c:cdl () 
  (princ 
    "\n****************************************************
     \n Developed by Peyman Bayat
     \n p2436b@gmail.com
     \n version  1.3
     \n Commands:
     \n cdlth => Setting font heigh
     \n cdl => Start drawing
     \n * if 'cdl' command is not working first set the font heigh with 'cdlth' command *
     \n****************************************************\n"
  )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace doc))
  (setvar "DIMZIN" 3)


  (setq texth (getenv "cdl_texth"))
  (if (not texth) 
    (progn 
      (setq th (getreal "\nEnter text heigh: "))
      (if th 
        (setenv "cdl_texth" (rtos th 2 1))
        (setenv "cdl_texth" 10)
      )
    )
  )

  ;; Prompt user for the base point
  (setq basePoint (getpoint 
                    "\nSelect the base point for distance measurements: "
                  )
  )
  (DrawPolylineWithText modelSpace basePoint "0.00")

  ;; Loop to get additional points
  (while 
    (setq otherPoint (getpoint 
                       "\nSelect a point to measure distance to the base point (or press Enter to finish): "
                     )
    )

    (setq dist (distance basePoint otherPoint))
    (DrawPolylineWithText modelSpace otherPoint (rtos (/ dist 100) 2 2))
  )

  (princ "\nMeasurement completed.")
  (princ)
)

(defun FixTextAngle (ang) 
  (if (and (> ang (* 0.5 pi)) (< ang (* 1.5 pi))) 
    (+ ang pi)
    ang
  )
)
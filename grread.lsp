(defun c:kit () 
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-modelspace doc))

  (while (setq pt (getpoint "\nEnter text location: ")) 
    (progn 
      (setq text (getstring "\Enter text: <This is a test>:"))
      (setq ikey (grread ()))
      (princ "\n***********************************\n")
      (princ ikey)
      (princ "\n***********************************\n")
      (setq textObj (vla-addtext modelSpace 
                                 text
                                 (vlax-3d-point (car pt) (cadr pt) 0.0)
                                 1
                    )
      )
    )
  )
  (princ)
)

(defun c:ChangeTextColor () 
  (prompt "\nSelect a text entity: ")
  ;; Let the user select a text entity
  (setq ent (car (entsel "\nSelect a text entity: ")))

  ;; Validate the selection
  (if (and ent (eq (cdr (assoc 0 (entget ent))) "TEXT")) 
    (progn 
      (prompt "\nUse the Up/Down arrow keys to change the color. Press ESC to exit.")
      ;; Define a list of colors to cycle through
      (setq colors '(1 2 3 4 5 6 7)) ; Standard AutoCAD color indices
      (setq color-index 0) ; Start with the first color

      ;; Start the loop
      (while T 
        (setq input (grread)) ; Capture user input
        (princ (cadr input))
        (cond 
          ;; Up arrow key (38) to go to the next color
          ((and (= (car input) 2) (= (cadr input) 43))
           (setq color-index (rem (+ color-index 1) (length colors))) ; Increment color index
           ;; Update the color of the text entity
           (entmod 
             (subst (cons 62 (nth color-index colors)) 
                    (assoc 62 (entget ent))
                    (entget ent)
             )
           )
           (entupd ent) ; Update the entity in the drawing
            (command "REGEN") ; Regenerate the drawing to reflect the change
           (prompt (strcat "\nColor changed to: " (itoa (nth color-index colors))))
          )

          ;; Down arrow key (40) to go to the previous color
          ((and (= (car input) 2) (= (cadr input) 45))
           (setq color-index (rem 
                               (+ color-index (1- (length colors)))
                               (length colors)
                             )
           ) ; Decrement color index
           ;; Update the color of the text entity
           (entmod 
             (subst (cons 62 (nth color-index colors)) 
                    (assoc 62 (entget ent))
                    (entget ent)
             )
           )
           (entupd ent) ; Update the entity in the drawing
            (command "REGEN") ; Regenerate the drawing to reflect the change
           (prompt (strcat "\nColor changed to: " (itoa (nth color-index colors))))
          )

          ;; ESC key (27) to exit the loop
          ((and (= (car input) 2) (= (cadr input) 27))
           (prompt "\nExiting color changer...")
           (exit)
          )
        ) ; Exit the loop
      )
    )
    ;; If the user did not select a valid text entity
    (prompt "\nInvalid selection. Please select a TEXT entity.")
  )
  (princ)
) ; End function

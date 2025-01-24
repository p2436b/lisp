;The contents of this file are subject to the Mozilla Public License
;Version 1.1 (the "License"); you may not use this file except in compliance
;with the License. You may obtain a copy of the License at
;http://www.mozilla.org/MPL/
;
;Software distributed under the License is distributed on an "AS IS" basis,
;WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
;the specific language governing rights and limitations under the License.
;
;The Original Code is: igneus.lsp
;This file is Copyright (c) 2006 Igneus Incorporated

;------------------------------------------------------------------------------
; The Igneus Incorporated Cad Utilities Collection
;
; History:
;     0.7.0 : April 24, 2006
;           : Initial public release
;
;     0.7.1 : October 12, 2006
;           : Metric support
;
; Commands added to CAD program
; -----------------------------
;
;           fpOptions       : set options for other commands
;           Pipedim         : draws size and length text for a line(s)
;           PipeLine        : A 'line' command with pipe dimensioning
;           TrimHeads       : Trim lines to edge of heads - don't use before
;                             ExportSHC.
;           ExportSHC       : produces a TSHC readable file from cad drawing
;           ImportSHC       : inputs a previously exported file and changes
;                           : pipe sizes accordingly
;
;------------------------------------------------------------------------------

;;;
;;;  FpOptions
;;;
;;;  Description
;;;  -----------
;;;  Sets options used by cad utilities collections and saves to
;;;  file.
;;;

(defun c:fpoptions( / s)
  ; Call initialization function
  (igneus_init)
  ; get tolerance
  (if (<= 0 (setq s (getreal (strcat "\nSet endpoint tolerance <" (rtos igneus_tolerance) ">:")))) (setq igneus_tolerance s))
  ; get branchline layer
  (setq igneus_BranchLayer (igneus_getLayName (strcat "Branchline piping layer <" igneus_BranchLayer ">:") igneus_BranchLayer))
  ; get main piping layer
  (setq igneus_MainLayer (igneus_getLayName (strcat "Main piping layer <" igneus_MainLayer ">:") igneus_MainLayer))
  ; get sprinkler head layer
  (setq igneus_HeadLayer (igneus_getLayName (strcat "Sprinkler head layer <" igneus_HeadLayer ">:") igneus_HeadLayer))
  ; get pipe dimensioning layer
  (setq igneus_PipeDimLayer (igneus_getLayName (strcat "Pipe dimensioning layer <" igneus_PipeDimLayer ">:") igneus_PipeDimLayer))
  ; get node label layer
  (setq igneus_NodeDimLayer (igneus_getLayName (strcat "Node labeling layer <" igneus_NodeDimLayer ">:") igneus_NodeDimLayer))
  ; select drawing units
  (setq igneus_BaseUnit (igneus_getintD (strcat "\nA length of 1.0 in the drawing is a - 1)Inch  2)Foot  3)Millimeter  4)Meter  <" (itoa igneus_baseUnit) ">:") igneus_baseUnit))
  ; Foot or inch base unit always results in Foot-Inch pipe length dimensioning
  (if (< igneus_BaseUnit 3)
    (progn
      ; get additional information for foot-inch dimensioning
      (setq igneus_LengthDimUnit 1)
      ; get 1/4 characters
      (setq igneus_oneQuarter (igneus_getstringD (strcat "Enter characters to use for '1/4' <" igneus_oneQuarter ">:") igneus_oneQuarter T))
      ; get 1/2 characters
      (setq igneus_oneHalf (igneus_getstringD (strcat "Enter characters to use for '1/2' <" igneus_oneHalf ">:") igneus_oneHalf T))
      ; get 3/4 characters
      (setq igneus_threeQuarter (igneus_getstringD (strcat "Enter characters to use for '3/4' <" igneus_threeQuarter ">:") igneus_threeQuarter T))
      ; get foot inch seperator
      (setq igneus_footchar (igneus_getstringD (strcat "Enter foot inch seperator character <" igneus_footchar ">:") igneus_footchar nil))
    )
    (progn
      ; select metric unit for length dimensioning
      (if (= 1 igneus_LengthDimUnit) (setq igneus_LengthDimUnit 2))
      (setq igneus_LengthDimUnit (igneus_getintD (strcat "\nDimension pipe lengths as - 2)Millimeter  3)Meter <" (itoa igneus_LengthDimUnit) ">:") igneus_LengthDimUnit))
    )
  )
  ; write options to "igneus.ini"
  (setq s (open "igneus.cfg" "w"))
  (write-line ";;; Igneus Cad Utilities configuration file" s)
  (write-line (strcat "(setq igneus_tolerance " (rtos igneus_tolerance 2 2) ")") s)
  (write-line (strcat "(setq igneus_BranchLayer \"" igneus_BranchLayer "\")") s)
  (write-line (strcat "(setq igneus_MainLayer \"" igneus_MainLayer "\")") s)
  (write-line (strcat "(setq igneus_HeadLayer \"" igneus_HeadLayer "\")") s)
  (write-line (strcat "(setq igneus_PipeDimLayer \"" igneus_PipeDimLayer "\")") s)
  (write-line (strcat "(setq igneus_NodeDimLayer \"" igneus_NodeDimLayer "\")") s)
  (write-line (strcat "(setq igneus_oneQuarter \"" igneus_oneQuarter "\")") s)
  (write-line (strcat "(setq igneus_oneHalf \"" igneus_oneHalf "\")") s)
  (write-line (strcat "(setq igneus_threeQuarter \"" igneus_threeQuarter "\")") s)
  (write-line (strcat "(setq igneus_footchar \"" igneus_footchar "\")") s)
  (write-line (strcat "(setq igneus_curPipeSize " (rtos igneus_curPipeSize 2 2) ")") s)
  (write-line (strcat "(setq igneus_BaseUnit " (itoa igneus_baseUnit) ")") s)
  (write-line (strcat "(setq igneus_LengthDimUnit " (itoa igneus_LengthDimUnit) ")") s)
  (close s)
  (igneus_end)
)

;;;
;;; PipeLine
;;;
;;; Description
;;; -----------
;;; Behaves as a LINE command but places lines in the branch line piping layer
;;; and dimensions each line using the chosen pipe size
;;;

(defun c:pipeline( / pt lastPt )
  ; Initialize
  (igneus_init)
  ; get pipe size
  (if (setq s (getreal (strcat "\nEnter pipe size <" (rtos igneus_curPipeSize 2 2) ">:")))
    (setq igneus_curPipeSize s))
  ; get starting point
  (if (setq lastPt (getpoint "\nSelect starting point of pipe:"))
    (while (setq pt (getpoint lastPt "\nselect end of pipe piece:"))
      (progn
        (if  (entmake (list (cons 0 '"LINE") (cons 8 igneus_branchLayer) (append '(10) lastPt) (append '(11) pt)))
          (pipedim_entity (entlast) igneus_curPipeSize igneus_footChar))
        (setq lastPt pt))))
  (igneus_end)
)

;;;
;;;  PipeDim
;;;
;;;  Description
;;;  -----------
;;;  PIPEDIM creates length and size dimension text for each pipe selected.
;;;

(defun c:pipedim( / pipedim_ss j)
  ; Call initialization function
  (igneus_init)
  ; get pipe size to use
  (if (null igneus_curPipeSize) (setq igneus_curPipeSize 1.0))
  (setq j igneus_curPipeSize)
  (if (null (setq igneus_curPipeSize (getReal (strcat '"Enter pipe size <" (rtos igneus_curPipeSize 2 2) '">:"))))
    (setq igneus_curPipeSize j))
  ; Let the use select the pipe to dimension
  (princ "\nSelect pipe to dimension")
  (setq pipedim_ss (ssget))

  ;;; Dimension each line in the selection set
  (setq j 0)
  (while (< j (ssLength pipedim_ss))
    (if (= '"LINE" (cdr (assoc 0 (entget (ssname pipedim_ss j)))))
	  (pipedim_entity (ssname pipedim_ss j) igneus_curPipeSize igneus_footchar))
    (setq j (+ j 1)) )
  (igneus_end)
)

;;;
;;;  TrimHeads
;;;
;;;  Description
;;;  -----------
;;;  Trims all pipe in the branchline and main layers against all
;;;  blocks in the sprinkler head layer.  This is not a true trim
;;; but sets endpoints of lines within the tolerance value to a
;;; block to the radius distance away from the center of the block
;;;

(defun c:TrimHeads( / tolerance trimRadius ssHeads ssPipe iHead iPipe eHead ePipe newX newY)
  ; Call initialization function
  (igneus_init)
  ; get sprinkler head block names
  (if (= "" (setq bNames (getstring "\nEnter sprinkler head block name(s) to trim against:"))) (quit))
  ; get trimming radius
  (while (= nil (setq trimRadius (getReal "\nEnter trimming radius:"))))
  ; create sprinkler head selection set
  (if (= nil (setq ssHeads (ssget "X" (list (cons 0 "INSERT") (cons 2 bNames)))))
    (progn
      (princ "\nNo sprinkler head blocks found.")
      (quit)))
  ; creat pipe selection set
  (if (= nil (setq ssPipe (ssget "X" (list (cons 0 "LINE") (cons 8 (strcat igneus_mainLayer "," igneus_branchLayer))))))
    (progn
      (princ (strcat "\nNo pipe found in layers " igneus_mainLayer " or " igneus_branchLayer))
      (quit)))
  ; cycle through each pipe with each head and trim if necessary
  (setq iHead 0)
  (while (< iHead (ssLength ssHeads))
    (setq eHead (entget (ssName ssHeads iHead)))
    (setq iPipe 0)
    (while (< iPipe (ssLength ssPipe))
      (setq ePipe (entget (ssName ssPipe iPipe)))
      ; check for line start point within head radius
      (if (>= igneus_tolerance (distance (cdr (assoc 10 ePipe)) (cdr (assoc 10 eHead))))
        (progn
          ; trim the line from start point
          (setq pAngle (angle (cdr (assoc 10 eHead)) (cdr (assoc 11 ePipe))))
          (setq newX (+ (cadr (assoc 10 eHead)) (* trimRadius (cos pAngle))))
          (setq newY (+ (caddr (assoc 10 eHead)) (* trimRadius (sin pAngle))))
          ; modify the line
          (entmod (subst (list 10 newX newY (last (assoc 10 ePipe))) (assoc 10 ePipe) ePipe))))
      ; check for line end point within head radius
      (if (>= igneus_tolerance (distance (cdr (assoc 11 ePipe)) (cdr (assoc 10 eHead))))
        (progn
          ; trim the line from end point
          (setq pAngle (angle (cdr (assoc 10 eHead)) (cdr (assoc 10 ePipe))))
          (setq newX (+ (cadr (assoc 10 eHead)) (* trimRadius (cos pAngle))))
          (setq newY (+ (caddr (assoc 10 eHead)) (* trimRadius (sin pAngle))))
          ; modify the line
          (entmod (subst (list 11 newX newY (last (assoc 11 ePipe))) (assoc 11 ePipe) ePipe))))
      (setq iPipe (1+ iPipe)))
    (setq iHead (1+ iHead)))
  (igneus_end)
)
  
;;;
;;;  ExportSHC
;;;
;;;  Description
;;;  -----------
;;;  Exports user selected pipe and flowing heads as a compatible
;;;  file for 'The Simple Hydraulic Calculator' computer program.
;;;
;;;  Limitations
;;;  -----------
;;;  This command does not attempt to place fitting codes in the resulting
;;;  file.  Nor does this command set pipe types or define a water source
;;;  In other words - the file will nead some editing in TSHC before it will
;;;  calc.  Still - it's a good time saver.
;;;

(defun c:exportSHC( / igneus_tolerance head_ss pipe_ss pipe_list node_list label_list command_list used_list head_count head_list fName j k x y newCommand)
  (igneus_init)		; Call initialization function
  (princ '"\nSelect pipe to export")
  (if (null (setq pipe_ss (ssget))) (progn
									  (*error* '"No pipe was selected")
									  (quit)))									
  ; get flowing heads (nil set is acceptable)
  (princ '"\nSelect flowing sprinkler heads")
  (setq head_ss (ssget))

  (setq node_list nil)
  (setq head_k -1)
  (setq head_q -1)

  ; cycle through all pipe selected
  ; for each line, add endpoints to node_list unless the endpoint already exists
  (setq j 0)
  (while (< j (sslength pipe_ss))
    (progn
	  ; get entity data
	  (setq k (entget (ssname pipe_ss j)))
	  (setq j (+ 1 j))
	  ; if its a line, add point to node_list (closer points first, farther second)
	  (if (= '"LINE" (cdr (assoc 0 k)))
	    (progn
          (if (> (distance '(0 0 0) (cdr (assoc 10 k))) (distance '(0 0 0) (cdr (assoc 11 k))))
            (setq x (cdr (assoc 11 k)) y (cdr (assoc 10 k)))
            (setq x (cdr (assoc 10 k)) y (cdr (assoc 11 k))))
		  (if (null (member x node_list))
		    (setq node_list (append node_list (list x))))
		  (if (null (member y node_list))
		    (setq node_list (append node_list (list y))))
  ))))
  ; search head selection set for sprinkler heads
  (setq head_count 0)
  (if (/= head_ss nil)
    (progn
	  ; get minimum discharge and k-factor
      (while (<= head_k 0)
	    (if (null (setq head_k (getreal '"Enter k-factor for sprinkler heads <5.6>:")))   ;;; change to 80.6 metric users
	      (setq head_k 5.6)))
      (while (<= head_q 0)
	    (if (null (setq head_q (getreal '"Enter minimum flow reaquired for a head <14.82>:"))) ;;; change to 57.0 metric users
	      (setq head_q 14.82)))
	  ;  Now add head points to node list
	  (setq j 0)
	  (while (< j (sslength head_ss))
	    (progn
	      ; get the entity data
	      (setq k (entget (ssname head_ss j)))
		  ; if its a block, add its point to the list
		  (if (= '"INSERT" (cdr (assoc 0 k)))
		    (progn
		      (setq node_list (append node_list (list (cdr (assoc 10 k)))))
			  (setq head_count (1+ head_count)) ))
		  ; increment j
		  (setq j (+ 1 j))
  ))))
  ; Now make a label list for the nodes
  ; Start numbering at 100
  (setq next_label '101)
  (setq label_list (list '100))
  (setq j 1)
  (while (< j (length node_list))
    (progn
	  ; compare current point with previous points
	  (setq k 0)
	  (while (< k j)
	    (if (< igneus_tolerance (abs (distance (nth k node_list) (nth j node_list))))
		  ;then
		  (setq k (1+ k))
		  ;else
		  (progn
		    ; points fall within tolerance so represent same node
			(setq label_list (append label_list (list (nth k label_list))))
			(setq k j)
	  )))
	  ;; if label list is too short then a duplicate was not found - add next label to list
	  (if (= j (length label_list))
	    (progn
		  (setq label_list (append label_list (list next_label)))
		  (setq next_label (1+ next_label))
	  ))
	  (setq j (1+ j))
  ))

  ; create a head label list so we know when we need a head command instead of node
  (setq head_list nil)
  (setq j (- (length label_list) head_count))
  (while (< j (length label_list))
    (progn
	  (setq head_list (append head_list (list (nth j label_list))))
	  (setq j (1+ j)) ))

  ; make a TSHC version 1.2 file header
  (setq command_list (list '"<TSHC 1 2>" '"<BODY>" '"// Pipe generated by exportshc command" '""))
  ; Now we can make pipe commands!
  (setq command_list (append command_list (list '"Use s40 120")))
  (setq next_label 100)
  (setq j 0)
  (while (< j (sslength pipe_ss))
    (progn
	  ; get entity data
	  (setq k (entget (ssname pipe_ss j)))
	  ; if its a line, add pipe command to command list
	  (if (= '"LINE" (cdr (assoc 0 k)))
	    (progn
		  ; get pipe size and length from dimension text if it exists
          (setq y (igneus_getPipeSizeLength (ssname pipe_ss j)))
          (setq x (car y))
          (if (= nil x) 
            (if (= 1 igneus_LengthDimUnit) (setq x "1.00") (setq x "25")))
          (setq y (cadr y))
          (if (= nil y)
            (progn            
              (setq y (igneus_rtos (distance (cdr (assoc 10 k)) (cdr (assoc 11 k))) '"'"))
              ; convert fraction characters to decimals for TSHC
              (setq y (igneus_subststr ".25" igneus_onequarter y))
              (setq y (igneus_subststr ".5" igneus_onehalf y))
              (setq y (igneus_subststr ".75" igneus_threequarter y))
              ; convert mm length to m for TSHC
              (if (= 2 igneus_lengthDimUnit) (setq y (rtos (/ (atof y) 1000.0) 2 3)))))
		  (setq newCommand
		    (strcat '"Pipe "
				  	(itoa next_label) 	; pipe name
					'" "                ; start node
					(itoa (nth (- (length node_list) (length (member (cdr (assoc 10 k)) node_list))) label_list))
					'" "			    ; end node
					(itoa (nth (- (length node_list) (length (member (cdr (assoc 11 k)) node_list))) label_list))
					'" " y))            ; length
		  ; extra spaces		
		  (while (> 26 (strlen newCommand)) (setq newCommand (strcat newCommand " ")))
          ; pipe size
          (setq newCommand (strcat newCommand '" " x '" "))
          ; extra spaces
		  (while (> 45 (strlen newCommand)) (setq newCommand (strcat newCommand " ")))
          ; entity name in comment
		  (setq newCommand (strcat newCommand "// $" (cdr (assoc 5 k))))
	      (setq command_list (append command_list (list newCommand)))
		  (setq next_label (1+ next_label))))
      (setq j (+ 1 j))

  ))
  ;  Now for the node commands
  (setq command_list (append command_list (list '"" '"// Nodes generated by exportshc command" '"")))
  (setq j 0)
  (setq used_list nil)
  (while (< j (length node_list))
    (progn
	  (if (not (member (nth j label_list) used_list))
	    (progn
		  ; First label node in drawing
		  (entmake (list
			         (cons 0 '"TEXT")
					 (cons 1 (strcat '"<" (itoa (nth j label_list)) '">"))
					 (cons 7 (getvar "TEXTSTYLE"))
					 (cons 8 igneus_nodeDimLayer) ;(getvar "CLAYER"))
					 (list 10 (car (nth j node_list))
					          (cadr (nth j node_list))
							  (caddr (nth j node_list)))
					 (list 11 (car (nth j node_list))
					          (cadr (nth j node_list))
							  (caddr (nth j node_list)))
					 (cons 40 (* (getvar "DIMSCALE") (getvar "DIMTXT")))
					 (cons 41 0.75)
					 (cons 50 0.0)
					 (cons 72 4)))
		  ; now the command
		  (setq used_list (append used_list (list (nth j label_list))))
		  (if (member (nth j label_list) head_list)
  		    ; make a head command
	        (setq command_list (append command_list (list (strcat '"Head "
		                                                          (itoa (nth j label_list)) '" "
															      (igneus_rtos (caddr (nth j node_list)) '"'") '" "
																  (rtos head_q 2 2) '" "
																  (rtos head_k 2 2)))))
		    ; make a node command
	        (setq command_list (append command_list (list (strcat '"Node "
		                                                          (itoa (nth j label_list)) '" "
															      (igneus_rtos (caddr (nth j node_list)) '"'")))))
	  )))
	  (setq j (1+ j))
  ))
  ; Get the filename
  (setq fName '"")
  (while (= fName '"") (setq fName (getfiled "Save As ..." "export.shc" "SHC" 1)))
  ; Open the file and write it
  (setq k (open fName "w"))
  (setq j 0)
  (while (< j (length command_list))
    (progn
	  (write-line (nth j command_list) k)
	  (setq j (1+ j))))
  (setq k (close k))
  (igneus_end)
)

;;;
;;; ImportSHC
;;;
;;; Reads a previously exported .shc file and dims the
;;; pipe in the drawing according to the size values in
;;; the .shc file.
;;;

(defun c:importSHC( / fName s comList )
  (igneus_init)
  ; Get the filename
  (setq fName '"")
  (setq s '"")
  (if (setq fName (getfiled "Import file ..." "" "SHC" 2))
  ; Open the file and read it
    (if (setq k (open fName "r"))
      (progn
	    ; get to command section of file : <BODY> tag
	    (while (and (/= nil s) (/= s "<BODY>")) (setq s (read-line k)))
        ; read until eof
	    (while (/= nil s)
	      (progn
		    ; check for Pipe command
            (setq comList nil)
		    (if (setq s (read-line k))			
		      (setq comList (igneus_strToTokens s)))
			(if (/= nil comList)
              (if (= (strcase (car comList)) "PIPE")
                (progn
  	  	          ; found a pipe, get size
                  (setq pSize (atof (nth 5 comList)))
			      ; entity handle should be last token
                  (setq hPipe (car (reverse comList)))
                  ; if first char is not $ then this is not a linked pipe
                  (if (= (substr hPipe 1 1) "$")
                    (progn
                      (setq hPipe (substr hPipe 2 20))
                      ;get pipe entity
                      (if (setq pList (handent hPipe))
                        (progn
                          (setq pList (entget pList '("IGNEUSINCUTILS")))
 			              ; retrieve size handle from extended data
                          (setq hSize (cdr (assoc 1005 (cdr (assoc "IGNEUSINCUTILS" (cdr (assoc -3 pList)))))))
			              ; retrieve size text entity list
			              (if (/= hSize nil) (setq sList (entget (handent hSize))) (setq sList nil))
			              (if (and (/= hSize nil) (/= sList nil))
			                ; modify existing text
			                (entmod (subst (cons 1 (igneus_rtos pSize nil)) (assoc 1 sList) sList))
			                ; no existing text, dim this pipe
			                (pipedim_entity (handent hPipe) pSize "-"))))))))))))))
  (igneus_end)
)

;;;
;;; Utility Functions used by commands.
;;; These should not be called directly.
;;;

;;; Error handler
(defun igneus_error (s)
  (princ (strcat "\nError: " s))
  (igneus_end)
)

;;; Initialization function for Igneus Inc. Utilities
(defun igneus_init ()
  ; Set new error handler
  (setq olderr *error*
        *error* igneus_error)
  ; Save system variables
  (setq curLayer (getvar "CLAYER"))
  (setq curBlip (getvar "BLIPMODE"))
  (setq curGrid (getvar "GRIDMODE"))
  (setq curHL (getvar "HIGHLIGHT"))
  (setq curCMD (getvar "CMDECHO"))
  ; Set system variables
  (setvar "CMDECHO" 0)
  (setvar "GRIDMODE" 0)
  (setvar "HIGHLIGHT" 1)
  ; Set beginning of an UNDO group
  (command "._UNDO" "_GROUP")
)

;;; Uninitialization function for Igneus Inc. Utilities
(defun igneus_end()
  ; Restore system variables
  (setvar "CLAYER" curLayer)
  (setvar "BLIPMODE" curBlip)
  (setvar "GRIDMODE" curGrid)
  (setvar "HIGHLIGHT" curHL)
  (command "._UNDO" "_E")
  (setvar "CMDECHO" curCMD)
  ; Restore original error handler
  (setq *error* olderr)
  (princ)
)

;;; This function takes a real number in igneus_baseunit units and converts
;;; it to a string formatted in accordance with igneus_lengthdimunit value.
;;; 
;;; formatted as: "<feet>'<inches>"
;;; The real number is treated as inches. And is rounded
;;; to the nearest quarter
(defun igneus_rtos(rlength footchar / rlength feet inches fraction fraction_string)
  (cond
    ;;; metric units always use mm pipe sizes - footchar is the flag
    ( (and (> igneus_lengthDimUnit 1) (= nil footchar)) (rtos rlength 2 0))
    ;;; meter
    ( (= igneus_lengthDimUnit 3)
      (progn
        ; Convert base unit to meters
        (cond ((= igneus_baseunit 1) (setq rlength (* rlength 0.0254)))
              ((= igneus_baseunit 2) (setq rlength (* rlength 0.3048)))
              ((= igneus_baseunit 3) (setq rlength (* rlength 0.001))))
        (rtos rlength 2 3))) ; 3 decimal places for m
    ;;; millimeter
    ( (= igneus_lengthDimUnit 2)
      (progn
        ; Convert base unit to millimeters
        (cond ((= igneus_baseunit 1) (setq rlength (* rlength 25.4)))
              ((= igneus_baseunit 2) (setq rlength (* rlength 304.8)))
              ((= igneus_baseunit 4) (setq rlength (* rlength 1000))))
        (rtos rlength 2 0)))  ; no decimal places for mm
    ;;; Foot-inch format
    ( (= igneus_lengthDimUnit 1)
      (progn
        ; Convert base unit to inches (for lengths)
        (if (/= nil footchar)
          (cond ((= igneus_baseunit 2) (setq rlength (* rlength 12)))
                ((= igneus_baseunit 3) (setq rlength (* rlength 0.039372)))
                ((= igneus_baseunit 4) (setq rlength (* rlength 39.372))))
        )
        (if (null footchar)
          (progn
            (setq feet 0)
            (setq inches (fix rLength))
           )
           (progn
             (setq feet (fix (/ rlength 12.0)))
             (setq inches (fix (- rlength (* feet 12))))
           )
        )
        (setq fraction (- rlength (fix rlength)))
        ; Round inches to nearest quarter (with 3/8 going to 1/2 and 7/8 going to 1)
        (setq fraction_string '"")
        (if (> fraction 0.126) (setq fraction_string igneus_onequarter))
        (if (> fraction 0.374) (setq fraction_string igneus_onehalf))
        (if (> fraction 0.626) (setq fraction_string igneus_threequarter))
        (if (> fraction 0.874) (progn
                                 (setq inches (+ inches 1))
                                 (setq fraction_string '"") ))
        (if (= inches 12)
            (progn
              (setq inches 0)
              (setq feet (+ feet 1)) ))
        ; Put feet & inches together
        (if (and (= feet 0) (null footchar))
          (strcat (itoa inches) fraction_string)
          (strcat (itoa feet) footchar (itoa inches) fraction_string)))))
)

;;;
;;; igneus_GetTextLinkHandle
;;;
;;; given a handle to text, returns handle
;;; from extended data of text intenty to line
;;;
(defun igneus_GetTextLinkHandle (hText) ; / nText
  (if (/= nil hText) (setq nText (handent hText)) (setq nText nil))
  (if (/= nil nText)
    (cdr (cadr (cadr (assoc -3 (entget nText '("IGNEUSINCUTILS"))))))))
  

;;;
;;;  Dimensioning Function ver 0.9
;;;
;;;  History
;;;  -------
;;;
;;;  Use:
;;;  ----
;;;  name: name of line entity to be dimensioned
;;;  pipesize: pipe size label text
;;;  footchar will be inserted between feet and inches on lengths
;;;  Draws text in current style on current layer
;;;

(defun pipedim_entity(eName pipeSize footchar );/
;                              pEnt          pStart     pEnd      hSize    hLength
;                              pAngle        pMid       sEntName  tempList
;                              pipedim_text  sEntList )

  ; Setup a TEXT Record
  (setq pipedim_text
    (list
      (cons  0 '"TEXT")                                     ; entity type
      ;(cons  8 (getvar "CLAYER"))                           ; layer (uses current layer)
      (cons 8 igneus_pipeDimLayer)
      (list 10 0.0 0.0 0.0)                                 ; Midpoint
      (list 11 0.0 0.0 0.0)                                 ; Midpoint
      (cons 40 (* (getvar "DIMSCALE") (getvar "DIMTXT")))   ; Height
      (cons  1 '"")                                         ; text value
      (cons 50 0.0)                                         ; text angle
      (cons 41 0.750)                                       ; Width factor
      (cons  7 (getvar "TEXTSTYLE"))                        ; text style
      (cons 72 4)                                           ; Textmode - midpoint
	  (cons -3 0)
    )
  )

  (setq pEnt (entget eName '("IGNEUSINCUTILS")))
  (if (= 'LINE' (cdr (assoc 0 pEnt))) (progn
									    (*error* '"Pipedim_entity not a line")
										(quit)))
  (setq pstart (cdr (assoc 10 pEnt)))
  (setq pend (cdr (assoc 11 pEnt)))
  ; Compute plan view angle of line (x and y coordinates only)
  (setq pangle (angle (reverse (cdr (reverse pstart))) (reverse (cdr (reverse pend)))))
  ; make sure angle isn't upside down
  (if (and (> pangle (/ pi 2)) (<= pangle (* 1.5 pi))) (setq pangle (- pangle pi)))
  ; Find the center point of the pipe
  (setq pmid (mapcar '/ (mapcar '+ pStart pEnd) '(2 2 2)))
  ; Create length string
  (setq pLength (distance pStart pEnd))
  ;(if (and (< pLength 11.875) (= igneus_LengthDimUnit 1) (/= nil footChar))
  ;  (setq pLength (strcat '"0" footchar (igneus_rtos pLength footchar)))
  ;  (setq pLength (igneus_rtos pLength footchar))
  ;)
  (setq pLength (igneus_rtos pLength footchar))
  ; get size and length text handles
  (setq hSize (cdr (assoc 1005 (cdr (assoc "IGNEUSINCUTILS" (cdr (assoc -3 pEnt)))))))
  (setq hLength (cdr (assoc 1005 (reverse (cdr (assoc "IGNEUSINCUTILS" (cdr (assoc -3 pEnt))))))))
  ; check for erased and improperly linked text entities (results from copying)
  (if (/= (cdr (assoc 5 pent)) (igneus_GetTextLinkHandle hSize)) (setq hSize nil))
  (if (/= (cdr (assoc 5 pent)) (igneus_GetTextLinkHandle hLength)) (setq hLength nil))
  ; See if there is an existing length text to modify
  (if (/= nil hLength)
    (progn
      ; There is a handle to a length entity - retrieve entity
      (setq sEntName (handent hLength))
      (setq sEntList (entget sEntName '("IGNEUSINCUTILS")))
      ; If this is not text, then error
      (if (/= '"TEXT" (cdr (assoc 0 sEntList))) (progn
												  (*error* '"pipe owns nontext - cad app/util conflict with Igneus")
												  (quit)))
      ; modify text to new value
      (setq sEntList (subst (cons 1 pLength) (assoc 1 sEntList) sEntList))
      (entmod sEntList))
    (progn
      ; no existing text, so make new
      ; Compute center point for text
      (setq pTxtCtr (list
                      (+ (car pMid) (* (sin pAngle) (* (getvar "DIMSCALE") (getvar "DIMTXT"))))
                      (- (cadr pMid) (* (cos pAngle) (* (getvar "DIMSCALE") (getvar "DIMTXT"))))
                      (caddr pMid)))
      ; Create the text entity
      (setq pipedim_text (subst (cons 1 pLength) (assoc 1 pipedim_text) pipedim_text))
      (setq pipedim_text
        (subst (append '(10) pTxtCtr) (assoc 10 pipedim_text) pipedim_text))
      (setq pipedim_text
        (subst (append '(11) pTxtCtr) (assoc 11 pipedim_text) pipedim_text))
      (setq pipedim_text (subst (cons 50 pAngle) (assoc 50 pipedim_text) pipedim_text))
	  ; link this text back to the pipe line entity
	  (setq pipedim_text (subst (cons -3 (list (list "IGNEUSINCUTILS" (cons 1005 (cdr (assoc 5 pent))))))
								(assoc -3 pipedim_text)
								pipedim_text))
      ; make the text and retrieve it
      (setq sEntList (entget (entmakex pipedim_text) '("IGNEUSINCUTILS")))
      ; store handle to text for later
	  (setq hLength (cdr (assoc 5 sEntList)))
    )
  )

  ; See if there is an existing size entity to modify
  (if (/= nil hSize)
    (progn
      ; There is a handle to a size entity - retrieve entity
      (setq sEntName (handent hSize))
      (setq sEntList (entget sEntName '("IGNEUSINCUTILS")))
      ; If this is not text, then error
      (if (/= '"TEXT" (cdr (assoc 0 sEntList))) (progn
												  (*error* '"pipe owns nontext - cad app/util conflict with Igneus Utils")
												  (quit)))
      ; modify text to new value
      (setq sEntList (subst (cons 1 (igneus_rtos pipeSize nil)) (assoc 1 sEntList) sEntList))
	  ; set pointer to length text entity
      (entmod sEntList))
    (progn
      ; Compute center point for text
      (setq pTxtCtr (list
                      (- (car pMid) (* (sin pAngle) (* (getvar "DIMSCALE") (getvar "DIMTXT"))))
                      (+ (cadr pMid) (* (cos pAngle) (* (getvar "DIMSCALE") (getvar "DIMTXT"))))
                      (caddr pMid)))
      ;;; Create the text entity
      (setq pipedim_text
        (subst (cons 1 (igneus_rtos pipeSize nil)) (assoc 1 pipedim_text) pipedim_text))
      (setq pipedim_text
        (subst (append '(10) pTxtCtr) (assoc 10 pipedim_text) pipedim_text))
      (setq pipedim_text
        (subst (append '(11) pTxtCtr) (assoc 11 pipedim_text) pipedim_text))
      (setq pipedim_text (subst (cons 50 pAngle) (assoc 50 pipedim_text) pipedim_text))
	  ; link this text back to the pipe line entity
	  (setq pipedim_text (subst (cons -3 (list (list "IGNEUSINCUTILS" (cons 1005 (cdr (assoc 5 pent))))))
								(assoc -3 pipedim_text)
								pipedim_text))
      ; make the text and retrieve it
      (setq sEntList (entget (entmakex pipedim_text) '("IGNEUSINCUTILS")))
      ; store handle to text for later
	  (setq hSize (cdr (assoc 5 sEntList)))
    )
  )
  ; Now store the handles for size and length text in line's extended data
  (if (setq tempList (cdr (assoc -3 pent)))
    ; line already has extended data
	(progn
	  (if (assoc "IGNEUSINCUTILS" tempList)
	    (setq tempList (subst (cons "IGNEUSINCUTILS" (list (cons 1005 hSize) (cons 1005 hLength)))
					          (assoc "IGNEUSINCUTILS" tempList)
							  tempList))
		(setq tempList (append tempList (list
										  (cons "IGNEUSINCUTILS" (list
																   (cons 1005 hSize)
																   (cons 1005 hLength)))))))
	  (setq pEnt (subst (cons -3 tempList) (assoc -3 pent) pent)))
	; Line does not have extended data
	(progn
	  (setq tempList (list (cons -3 (list (cons "IGNEUSINCUTILS" (list (cons 1005 hSize) (cons 1005 hLength)))))))
	  (setq pEnt (append pEnt tempList))))
  ; modify the line entity
  (entmod pent)
)

;;;
;;; Seperates string into tokens and returns as list
;;;
(defun igneus_strToTokens(s );/ tokens charList i j)
  ; find tokens
  (setq tokens nil)
  (setq i 1)
  (setq j 0)
  (while (<= i (strlen s))
    (progn
	  ; find start of token
	  (while (and (<= i (strlen s)) (= (substr s i 1) " ")) (setq i (1+ i)))
	  ; find end of token
	  (setq j (1+ i))
	  (while (and (<= j (strlen s)) (/= (substr s j 1) " ")) (setq j (1+ j)))
	  (if (<= i (strlen s)) (setq tokens (append tokens (list (substr s i (- j i))))))
	  (setq i (1+ j))))
  ; return list
  (setq tokens tokens)
)

(defun igneus_getStringD( p d flag / s)
  (if (= "" (setq s (getString flag p))) (setq s d))
  (setq s s))
  
(defun igneus_getintD( p d / s)
  (if (= nil (setq s (getint p))) (setq s d))
  (setq s s))

(defun igneus_getLayName( p d / s )
  (while (not (tblsearch "layer" (setq s (igneus_getstringD p d T)))))
  (setq s s)
)

;;;
;;; Sorts a list of reals/integers using the quick sort algorithm
;;;
(defun igneus_rqsort ( values / values lower_set upper_set dividor )
  ;;; If there is 1 or fewer elements in the list then just return it.  
  (if (< (length values) 2)
    values
    (progn
      ;;; User the average of the first & last values as the dividor value
      (setq dividor (/ (+ (car values) (last values)) 2.0))
      ;;; Initialize the lower & upper sets to the empty list
      (setq lower_set '())
      (setq upper_set '())
      ;;; Split the values into lower & upper ranges
      (while (> (length values) 0)
        (if (> (car values) dividor)
          (setq upper_set (append upper_set (list (car values))))
          (setq lower_set (append lower_set (list (car values)))))
        (setq values (cdr values)))
      ;;; If no split occured then first & last are equal and maximums
      ;;; Take one & put in upper_set so sorting may continue.
      (if (= 0 (length upper_set))
        (setq upper_set (list (car lower_set)) lower_set (cdr lower_set)))
      (append (igneus_rqsort lower_set) (igneus_rqsort upper_set)) ; tail recursion
    )
  )
)

;;;
;;; Sorts a list of lists of reals/integers by the indexed
;;; element in each sublist using the quick sort algorithm.
;;;
(defun igneus_rqsortn ( values index / values lower_set upper_set dividor )
  ;;; If there is 1 or fewer elements in the list then just return it.  
  (if (< (length values) 2)
    values
    (progn
      ;;; User the average of the first & last values as the dividor value
      (setq dividor (/ (+ (nth index (car values)) (nth index(last values))) 2.0))
      ;;; Initialize the lower & upper sets to the empty list
      (setq lower_set '())
      (setq upper_set '())
      ;;; Split the values into lower & upper ranges
      (while (> (length values) 0)
        (if (> (nth index (car values)) dividor)
          (setq upper_set (append upper_set (list (car values))))
          (setq lower_set (append lower_set (list (car values)))))
        (setq values (cdr values)))
      ;;; If no split occured then first & last are equal and maximums
      ;;; Take one & put in upper_set so sorting may continue.
      (if (= 0 (length upper_set))
        (setq upper_set (list (car lower_set)) lower_set (cdr lower_set)))
      (append (igneus_rqsortn lower_set index) (igneus_rqsortn upper_set index)) ; tail recursion
    )
  )
)

;;;
;;; substitutes new substring for old substring in string
;;;
(defun igneus_subststr(a b s / i j)
  (setq i 1)
  (while (<= i (1+ (- (strlen s) (strlen b))))
    (if (= b (substr s i (strlen b)))
      (setq s (strcat
                (substr s 1 (1- i))
                a
                (substr s (+ i (strlen b))))))
    (setq i (1+ i)))
  (setq s s)
)

;;;
;;; gets size and length text for a pipe if it already exists
;;;
(defun igneus_getPipeSizeLength(nPipe / nPipe lPipe hSize hLength sEntName sEntList)
  ; get pipe entity list
  (setq lPipe (entget nPipe '("IGNEUSINCUTILS")))
  ; get size and length text handles
  (setq hSize (cdr (assoc 1005 (cdr (assoc "IGNEUSINCUTILS" (cdr (assoc -3 lPipe)))))))
  (setq hLength (cdr (assoc 1005 (reverse (cdr (assoc "IGNEUSINCUTILS" (cdr (assoc -3 lPipe))))))))
  ; check for erased and improperly linked text entities (results from copying)
  (if (/= (cdr (assoc 5 lPipe)) (igneus_GetTextLinkHandle hSize)) (setq hSize nil))
  (if (/= (cdr (assoc 5 lPipe)) (igneus_GetTextLinkHandle hLength)) (setq hLength nil))
  ; Get length text if it exists
  (if (/= nil hLength)
    (progn
      ; There is a handle to a length entity - retrieve entity
      (setq sEntName (handent hLength))
      (setq sEntList (entget sEntName '("IGNEUSINCUTILS")))
      ; If this is not text, then error
      (if (/= '"TEXT" (cdr (assoc 0 sEntList))) (progn
												  (*error* '"pipe owns nontext - cad app/util conflict with Igneus")
												  (quit)))
      ; get text
      (setq hLength (cdr (assoc 1 sEntList)))
      ; substitue TSHC characters for user defined characters
      (setq hLength (igneus_subststr "'" igneus_footchar hLength))
      (setq hLength (igneus_subststr ".25" igneus_onequarter hLength))
      (setq hLength (igneus_subststr ".5" igneus_onehalf hLength))
      (setq hLength (igneus_subststr ".75" igneus_threequarter hLength))
      ; if display units are mm, convert to m for The Simple Hydraulic Calculator
      (if (= 2 igneus_lengthDimUnit) (setq hLength (rtos (/ (atof hLength) 1000.0) 2 3)))))

  ; get size text if it exists
  (if (/= nil hSize)
    (progn
      ; There is a handle to a size entity - retrieve entity
      (setq sEntName (handent hSize))
      (setq sEntList (entget sEntName '("IGNEUSINCUTILS")))
      ; If this is not text, then error
      (if (/= '"TEXT" (cdr (assoc 0 sEntList))) (progn
												  (*error* '"pipe owns nontext - cad app/util conflict with Igneus Utils")
												  (quit)))
      ; modify text to new value
      (setq hSize (cdr (assoc 1 sEntList)))
      ; substitue TSHC characters for user defined characters
      (setq hSize (igneus_subststr "'" igneus_footchar hSize))
      (setq hSize (igneus_subststr ".25" igneus_onequarter hSize))
      (setq hSize (igneus_subststr ".5" igneus_onehalf hSize))
      (setq hSize (igneus_subststr ".75" igneus_threequarter hSize))))      
  ; return list of size and length
  (list hSize hLength)
)

; default settings
(setq igneus_BranchLayer "0")
(setq igneus_MainLayer "0")
(setq igneus_HeadLayer "0")
(setq igneus_PipeDimLayer "0")
(setq igneus_NodeDimLayer "0")
(setq igneus_onequarter ".25")
(setq igneus_onehalf ".5")
(setq igneus_threequarter ".75")
(setq igneus_footchar "-")
(setq igneus_curPipeSize 1.0)    ; default pipe size of 1"
(setq igneus_tolerance 1.0)      ; default tolerance of 1"
(setq igneus_BaseUnit 1)         ; default to base unit of inch
(setq igneus_LengthDimUnit 1)    ; default to foot-inch length dimensioning

; load user settings if they exist
;(load "igneus.cfg" "")

(setq igneus_tolerance 1)                            ; end point tolerance
(setq igneus_BranchLayer "0")                    ; branchline layer
(setq igneus_MainLayer "0")                    ; fire main layer
(setq igneus_HeadLayer "0")                        ; sprinkler head layer
(setq igneus_PipeDimLayer "0")                    ; pipe dimensioning layer
(setq igneus_NodeDimLayer "0")                ; hydraulic calculation node layer
(setq igneus_oneQuarter ".25")                        ; one quarter string
(setq igneus_oneHalf ".5")                            ; one half string
(setq igneus_threeQuarter ".75")                    ; three quarter string
(setq igneus_footchar "-")                            ; foot,inch seperator
(setq igneus_curPipeSize igneus_curPipeSize)
(setq igneus_BaseUnit 3)                            ; base unit of measure (1 inch, 2 foot, 3 mm, 4 m)
(setq igneus_LengthDimUnit 2)    ; dimension length (1 foot, 2 mm, 3 m)



;;;---------------------------------------------------------------------------
(regapp "IGNEUSINCUTILS")
(princ "\nIgneus Inc Cad Utilities v0.7.1 Loaded. (c)2006\n")
(princ)
(defun c:talong(/ actDoc actSp cText curAng curDer curPar
	curStr curTxt lChr oldMode oldOff oldSize
	oldSnap pt1 pt2 rLst selObj selPt stFlag
	tmpLn tStr txTpt unStart whatDo)
 (vl-load-com)
 (defun asmi_EntselWithOptions(Message / grLst filPt selSet)
 (if Message
   (princ Message)
   (princ "\nSelect object: ")
   ); end if
    (setq lChr ""
	  grLst(list 2 678)
	  tStr ""
	  ); end setq
    (while
      (and
          (not
	    (member lChr '(" " "\r")))
	  (/= 3(car grLst))
	); end or
  (if
    (setq grLst(grread nil 4 2))
    (progn
      (cond
	  ((= 3(car grLst))
	   (setq filPt(cadr grLst)
	         selSet(ssget filPt)
	         ); end setq
	   (if selSet
	       (setq outVal
	        (list(ssname selSet 0)filPt))
	     ); end if
	   ); end cond #1
	  ((= 2(car grLst))
	   (setq lChr(chr(cadr grLst)))
	   (if
	     (not
	       (member lChr '(" " "\r")))
		     (progn
		     (setq tStr(strcat tStr lChr)
			   outVal tStr); end setq
		     (princ lChr)
	       ); end progn
	     ); end if
	   ); end cond #2
        ); end cond
       ); end progn
      ); end if
     ); end while
    outVal
   ); end of asmi_EntselWithOptions

 (defun asmi_LayersUnlock(/ restLst)
 (setq restLst '())
 (vlax-for lay
   (vla-get-Layers
            (vla-get-ActiveDocument
              (vlax-get-acad-object)))
   (setq restLst
    (append restLst
      (list
        (list
         lay
          (vla-get-Lock lay)
  (vla-get-Freeze lay)
         ); end list
        ); end list
      ); end append
   ); end setq
   (vla-put-Lock lay :vlax-false)
   (if
     (vl-catch-all-error-p
(vl-catch-all-apply
 'vla-put-Freeze(list lay :vlax-false)))
     t)
   ); end vlax-for
 restLst
 ); end of asmi_LayersUnlock

 (defun asmi_LayersStateRestore(StateList)
 (foreach lay StateList
   (vla-put-Lock(car lay)(cadr lay))
    (if
     (vl-catch-all-error-p
(vl-catch-all-apply
  'vla-put-Freeze(list(car lay)(nth 2 lay))))
      t)
   ); end foreach
 (princ)
    ); end of asmi_LayersStateRestore

 
;;===========================================================
;; UNFORMAT.LSP (c)2003, John F. Uhden, Cadlantic/CADvantage
;; v1.0 (04-01-03)
;; Removes MTEXT formatting with option to retain the "\\P" LineFeeds
;;
;; Arguments:
;; Mtext - either an Ename or VLA-Object
;; KeepLF - nil (discard LineFeeds) non-nil (retain LineFeeds)
;;
;; NOTES:
;; Only R15 or higher.
;; v1.0 is only the first attempt.
;; We can always embellish the code with additional options.
;; Yes, it can probably be sped up using integers, but this is legible.
;;
(defun UnFormat (Mtext KeepLF / Text Str)
(vl-load-com)
 (cond
    ((= (type Mtext) 'VLA-Object)); end condition #1
    ((= (type Mtext) 'ENAME)
     (setq Mtext (vlax-ename->vla-object Mtext))
     ); end condition #2
   (1 (setq Mtext nil)) ; end condition #3
 ); end cond
    (and
  Mtext
  (= (vlax-get Mtext 'ObjectName) "AcDbMText")
  (setq Mtext (vlax-get Mtext 'TextString))
  (setq Text "")
(while (/= Mtext "")
 (cond
   ((wcmatch (strcase
	(setq Str
	   (substr Mtext 1 2))) "\\[\\{}`~]")
    (setq Mtext (substr Mtext 3)
          Text (strcat Text Str)
          ); end setq
      ); ed condition #1
   ((wcmatch (substr Mtext 1 1) "[{}]")
     (setq Mtext (substr Mtext 2))
     ); end condition #2
   ((and KeepLF (wcmatch (strcase (substr Mtext 1 2)) "\\P"))
     (setq Mtext (substr Mtext 3)
           Text (strcat Text "\\P")
           ); end setq
     ); end condition #3
   ((wcmatch (strcase (substr Mtext 1 2)) "\\[LOP]")
     (setq Mtext (substr Mtext 3))
     ); end condition #4
    ((wcmatch (strcase (substr Mtext 1 2)) "\\[ACFHQTW]")
       (setq Mtext
      (substr Mtext (+ 2 (vl-string-search ";" Mtext))))
     ); end condition #5
    ((wcmatch (strcase (substr Mtext 1 2)) "\\S")
       (setq Str
       (substr Mtext 3
	       (- (vl-string-search ";" Mtext) 2))
             Text (strcat Text
		   (vl-string-translate "#^\\" " " Str))
             Mtext (substr Mtext (+ 4 (strlen Str)))
             ); end setq
        (print Str)
      ); end condition #6
    (1
      (setq Text (strcat Text (substr Mtext 1 1))
            Mtext (substr Mtext 2)
           ); end setq
      ); end condition #7
    ); end cond
 ); end while
); end and
 Text
); end of UnFormat

 (defun asmi_GetActiveSpace(/ actDoc spFlag)
     (setq actDoc(vla-get-ActiveDocument
                    (vlax-get-acad-object))
           spFlag(vla-get-ActiveSpace actDoc)
          ); end setq
      (if(= 0 spFlag)
         (setq actSp(vla-get-PaperSpace actDoc))
         (setq actSp(vla-get-ModelSpace actDoc))
       ); end if
   ); end of asmi_GetActiveSpace
 (asmi_GetActiveSpace)

 (defun EnvironmentRestore()
   (if oldSnap
     (setvar "OSMODE" oldSnap)
     ); end if
   (if unStart
     (vla-EndUndoMark actDoc)
     ); end if
   (if tmpLn
     (vla-Delete tmpLn)
     ); end if
   (if rLst
     (asmi_LayersStateRestore rLst)
     ); end if
   (if selObj
     (vla-Highlight selObj :vlax-false)
     ); end if
   (princ)
   ); end of EnvironmentRestore

 (defun *error*(msg)
   (EnvironmentRestore)
   (princ "\n<<< Console break. Quit. >>> ")
   (princ)
   ); end of *error*
 
 (if(not tal:mode)(setq tal:mode "Type"))
 (if(not tal:off)(setq tal:off 1.5))
 (if(not tal:size)(setq tal:size(getvar "TEXTSIZE")))
 (setq oldSnap(getvar "OSMODE"))
 (while
     (and
(/= 'LIST(type whatDo))
(not stFlag)
     ); end or
     (princ
       (strcat
     "\n<<< Mode = " tal:Mode ", Text size = " (rtos tal:size)
     ", Offset = " (rtos tal:off) " >>> ") ; end strcat
      ); end princ
 (setq whatDo
 (asmi_EntselWithOptions
   "\nSelect curve or [settings/Quit] > ")
); end setq
   (cond
     ((= 'LIST(type whatDo))
      (setq selObj
      (vlax-ename->vla-object
	(car whatDo))
     selPt T
     txtPt T
     ); end setq
      (if
 (member
   (vla-get-ObjectName selObj)
   '("AcDbLine" "AcDbPolyline" "AcDb3dPolyline"
     "AcDbSpline" "AcDbCircle" "AcDbEllipse"
     "AcDbArc" "AcDbRay" "AcDbXline")
   ); end menber
 (progn
   (vla-Highlight selObj :vlax-true)
   (setq rLst(asmi_LayersUnlock))
   (while
     (and selPt txtPt)
     (vla-StartUndoMark
       (setq actDoc
	  (vla-get-ActiveDocument
	    (vlax-get-acad-object))))
     (setq unStart T)
     (setvar "OSMODE" 3071)
     (if
       (setq selPt
	    (getpoint
	      "\nPick point on curve or Right Click to Quit > "); end getpoint
	   ); end setq
       (progn
     (if
       (setq curPar
	      (vlax-curve-GetParamAtPoint selObj
		(setq selPt(trans selPt 1 0))))
       (progn
	  (setq curDer
		 (vlax-curve-GetFirstDeriv selObj
                             curPar)
		); end setq
                (if(=(cadr curDer) 0.0)
                   (setq curAng (/ pi 2))
                     (setq curAng
			(- pi
			 (atan
			  (/(car curDer)
			    (cadr curDer)))))
                  ); end if
	 (setq pt1
		(polar selPt curAng (* tal:size tal:off))
	       pt2
		(polar selPt curAng (-(* tal:size tal:off)))
	       tmpLn(vla-AddLine actSp
		      (vlax-3D-point pt1)(vlax-3D-point pt2)
		      ); end vla-AddLine
	       ); end setq
	 (vla-put-Color tmpLn acRed)
	 (setvar "OSMODE" 1)
	 (if
	   (setq txtPt
		  (getpoint
		    "\nPick middle point of text or Right Click to Quit > ")
		 ); end setq
	   (progn
	     (setq txtPt
		    (vlax-3d-point
		      (trans txtPt 1 0))
		   curStr nil); end setq
	     (while(not curStr)
	     (if
	       (= tal:mode "Type")
	       (progn
	         (setq curStr
		      (getstring T
			"\nEnter text: "); end getstring
		     ); end setq
		 (if(= "" curStr)(setq curStr nil))
	       ); end progn
	       (progn
		 (if
		   (and
		     (setq cText
			  (nentsel
			    "\nCopy text > "))
		     (setq cText
			    (vlax-ename->vla-object(car cText)))
		     (member
		       (vla-get-ObjectName cText)
		       '("AcDbText" "AcDbMText" "AcDbAttribute")
		       ); end member
		   ); end and
		   (if
		     (= "AcDbMText"
			(vla-get-ObjectName cText))
		       (setq curStr
			    (UnFormat cText nil)); end setq
		       (setq curStr
			      (vla-get-TextString cText)); end setq
		     ); end if
	           ); end if
		 ); end progn
	       ); end if
	       (if(not curStr)
		 (princ "\n>>> Empty input! <<< ")
		 (progn
		 (setq curTxt
			(vla-addText actSp curStr
			  txtPt tal:size)); end setq
		 (if
		   (and(< curAng(* 2 pi))(> curAng pi))
			 (vla-put-Rotation curTxt (+ curAng(/ pi 2)))
			 (vla-put-Rotation curTxt (- curAng(/ pi 2)))
		   ); end if
			 (vla-put-Alignment curTxt acAlignmentMiddleCenter)
			 (vla-Move curTxt
				   (vla-get-TextAlignmentPoint curTxt)
				   txtPt); end move
		   ); end progn
		 ); end if
	      ); end while
	     ); end progn
	   (princ "\n<<< Quit >>> ")
	   ); end if
     		 (vla-Delete tmpLn)
	 (setq tmpLn nil)
	 ); end progn
        (progn
         (princ "\n>>> Point isn't at curve! Quit. <<< ")
	 (setq selPt nil)
        ); end progn
       ); end if
     ); end progn
       (princ "\n<<< Quit >>> ")
       ); end if
     (vla-EndUndoMark actDoc)
     (setq unStart nil)
     ); end while
   (vla-Highlight selObj :vlax-false)
   (asmi_LayersStateRestore rLst)
   ); end progn
 (princ "\n>>> This isn't curve! Quit. <<< ")
 ); end if
      ); end condition #1
     ((= "S" (strcase whatDo))
      (initget "Type Copy")
       (setq oldMode tal:mode
      oldOff tal:off
      oldSize tal:size
      tal:mode
       (getkword
	 (strcat
	   "\nSpecify text creation mode [Type/Copy] <"
	   tal:mode ">: "); end strcat
	 ); end getkword
      tal:size
       (getreal
	 (strcat
	   "\nSpecify text size <"
	   (rtos tal:size) ">: "); end strcat
	 ); end getreal
      tal:off
       (getreal
	 (strcat
	   "\nSpecify text offset from line. TEXT SIZE * <"
	   (rtos tal:off) ">: "); end strcat
	 ); end getreal
      ); end setq
      (if(null tal:mode)(setq tal:mode oldMode))
      (if(null tal:size)(setq tal:size oldSize))
      (if(null tal:off)(setq tal:off oldOff))
      ); end condition #2
     ((= "Q" (strcase whatDo))
      (princ "\n<<< Quit >>> ")
      (setq stFlag T)
      ); end condition #3
     (T
       (princ "\nInvalid option keyword. ")
      ); end condition #4
     ); end cond
   ); end while
 (EnvironmentRestore)
 (princ)
 ); end of c:talong




 ;;  TextAlignWithObject.lsp
;;  CAB   02/19/2007
;;
;;  Add text to DWG at angle of selected object

(defun c:tao() (c:TextAlignWithObject)) ; shortcut

(defun c:TextAlignWithObject (/ tmp ang p@pt parA parB pt start txtht 
              FixTextAngle addtext)
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
   (setq txtObj
          (vla-addtext
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
   (vla-put-alignment txtObj acalignmentbottomcenter)
   (vla-put-textalignmentpoint txtobj (vlax-3d-point ipt))

 )

 ;;  -=< START HERE  >=-
 
 ;;  Get text string to insert
 (or txtstr (setq txtstr "Default Text"))
 (if (/= (setq tmp (getstring t (strcat "\nEnter text string: < " txtstr " > "))) "")
   (setq txtstr tmp)
 )

 ;;  Get object to align text & insert point
 ;;  Object must have curve data
 (if (and (setq ent (entsel "\nSelect point on object to label."))
          (not (vl-catch-all-error-p
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
                )  ; aprox angle of curve at pick point
           ang  (FixTextAngle ang)
     )
     ;;  Text height by style or current Text Size
     (if (zerop (setq txtht (getvar 'textsize)))
       (setq txtht (getvar "TextSize"))
     )
     (addtext pt txtht txtstr ang (getvar "clayer")) ; ins hgt text ang 
   )
   (prompt "\n**  Missed or no curve data for object.")
 )
 (princ)
)
(prompt "\nTextAlignWithObject.lsp loaded enter TAO to run.")
(princ)
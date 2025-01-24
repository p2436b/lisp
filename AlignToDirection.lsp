;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                      Align To Direction                                      ;;;
;;;                                 Created by Jonathan Handojo                                  ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;  This routine allows users to align any objects to a certain direction that contains any     ;;;
;;;  one of the following properties:                                                            ;;;
;;;                                                                                              ;;;
;;;  1. Insertion Point                                                                          ;;;
;;;  2. Text Position                                                                            ;;;
;;;  3. Text Alignment Position                                                                  ;;;
;;;                                                                                              ;;;
;;;  Upon issuing the command ATD, the user will be prompted a selection of objects without any  ;;;
;;;  filters. This will then be filtered after the selection set, highlighting the list of       ;;;
;;;  objects to be aligned. The user will be prompted the direction by clicking two point, or    ;;;
;;;  one of the following options:                                                               ;;;
;;;                                                                                              ;;;
;;;  1. X-direction,                                                                             ;;;
;;;  2. Y-direction (relative to the UCS)                                                        ;;;
;;;  3. "PerpendicularToCurve" - the user clicks a point on any curve objects and the            ;;;
;;;  direction will be perpendicular to the curve at that specified point.                       ;;;
;;;  4. "AlongCurve" - similar to step 3, but as opposed to perpendicular, it will be the        ;;;
;;;  direction of the curve at that specified point.                                             ;;;
;;;                                                                                              ;;;
;;;  Curve Selection is compatible with all curves within complex objects, regardless of depth.  ;;;
;;;  Several examples include curves found in nested blocks, xrefs, tables, dimensions, etc.     ;;;
;;;                                                                                              ;;;
;;;                                                                                              ;;;
;;;  Following this step, the user can choose to specify an optional distance that would like    ;;;
;;;  to be used as an "offset". Exactly taking up the feature from the built-in AutoCAD          ;;;
;;;  "OFFSET" command, this prompt offers the "Through" option in which the user then drags the  ;;;
;;;  mouse and click on a point to specify the location. If the user has chosed to specify a     ;;;
;;;  distance, the user will then be prompted to specify a point from which the offset will be   ;;;
;;;  calculated from.                                                                            ;;;
;;;                                                                                              ;;;
;;;  The user can next specify the point where all the objects will be aligned to that           ;;;
;;;  direction. Simply put, it's like aligning marbles on a table using a ruler. Except unlike   ;;;
;;;  marbles, AutoCAD entities do not keep rolling and stick to your cursor. Should the user     ;;;
;;;  have specified zero distance, the program will align all the objects along that point and   ;;;
;;;  the command will immediately terminate. Should the user have specified a positive distance, ;;;
;;;  the user can then specify a point denoting the side to place the objects.                   ;;;
;;;                                                                                              ;;;
;;;  Objects that I have successfully worked with this program include:                          ;;;
;;;                                                                                              ;;;
;;;  1. Tables                                                                                   ;;;
;;;  2. Dimensions                                                                               ;;;
;;;  3. Blocks                                                                                   ;;;
;;;  4. OLE Images                                                                               ;;;
;;;  5. Texts and MTexts (regardless of justification)                                           ;;;
;;;                                                                                              ;;;
;;;  This program will work under any UCS.                                                       ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                     Versions and updates                                     ;;;
;;;                 ------------------------------------------------------------                 ;;;
;;;                                                                                              ;;;
;;;  Version 1.0 - First release (27/04/20)                                                      ;;;
;;;                                                                                              ;;;
;;;  Version 1.1 (23/11/2020) - Included "Perpendicular to Curve" and "Along Curve" option.      ;;;
;;;                                                                                              ;;;
;;;  Version 1.2 (06/10/2021) - Curve selection enhanced compatibility with nested complex       ;;;
;;;                             objects and blocks.                                              ;;;
;;;                                                                                              ;;;
;;;  Version 1.2 (05/05/2023) - Command to now work regardless of elevation of objects on any    ;;;
;;;                             plane.                                                           ;;;
;;;                           - Included a feature to align objects at a specified distance      ;;;
;;;                             from a point.                                                    ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:atd (/ *error* acadobj activeundo adoc alpts ang ang1 ang2 blk gr grp grv 
                halfpi movpt msg msp p1 p2 p3 pg pt snp snpang ss sse x y
             )
    (defun *error* ( msg )
        (setvar 'snapang snp)
        (if sse (mapcar '(lambda (x) (redraw x 4)) sse))
        (vla-EndUndoMark adoc)
        (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
            (princ (strcat "Error: " msg))
        )
    )
    
    (defun _inters (ll ur p1 p2)
        (vl-some
            (function
                (lambda (a b)
                    (inters a b p1 p2)
                )
            )
            (list ll (list (car ur) (cadr ll)) ur (list (car ll) (cadr ur)))
            (list (list (car ur) (cadr ll)) ur (list (car ll) (cadr ur)) ll)
        )
    )
    (setq acadobj    (vlax-get-acad-object)
          adoc       (vla-get-ActiveDocument acadobj)
          blk        (vla-get-blocks adoc)
          msp        (vla-get-ModelSpace adoc)
          activeundo nil
    )
    (if (= 0 (logand 8 (getvar "UNDOCTL"))) (vla-StartUndoMark adoc) (setq activeundo T))
    
    (setq snp (getvar 'snapang))
    (if
        (setq ss 
            (vl-remove-if-not
                '(lambda (x)
                    (and
                        (vlax-write-enabled-p x)
                        (or
                            (vlax-property-available-p x 'insertionpoint)
                            (and
                                (eq (strcase (vla-get-ObjectName x)) "ACDBBLOCKREFERENCE")
                                (eq (vla-get-IsXRef (vla-item blk (vla-get-EffectiveName x))) :vlax-false)
                            )
                            (vlax-property-available-p x 'textposition)
                            (vlax-property-available-p x 'textalignmentpoint)
                            (vlax-property-available-p x 'center)
                        )
                    )
                )
                (atd:selset-to-list-vla (ssget "_:L"))
            )
        )
        (progn
            (setq sse (mapcar 'vlax-vla-object->ename ss))
            (mapcar '(lambda (x) (redraw x 3)) sse)
            (setq 
                ang (progn (initget 1 "X Y PErpendicularToCurve ALongCurve") (getangle "\nSpecify alignment direction [X/Y/PErpendicularToCurve/ALongCurve]: "))
                ang (+
                        (setq halfpi (* 0.5 pi))
                        (cond
                            (   (numberp ang) ang)
                            (   (eq ang "X") 0)
                            (   (eq ang "Y") halfpi)
                            (   (wcmatch ang "PErpendicularToCurve,ALongCurve")
                                (+
                                    (atd:GetDirectionAtCurve "\nSpecify point at any curve: ")
                                    (if (eq ang "PErpendicularToCurve") halfpi 0)
                                )
                            )
                        )
                    )
                p1 (trans (vlax-get (car ss) (atd:objprop (car ss))) 0 1)
                p2 (polar p1 ang 1)
                alpts 
                    (mapcar
                        '(lambda (x / p3)
                            (list x
                                (atd:objprop x)
                                (   (lambda (y)
                                        (list (car y) (cadr y) (caddr p3))
                                    )
                                    (inters 
                                        (atd:3p->2p p1)
                                        (atd:3p->2p p2)
                                        (atd:3p->2p (setq p3 (trans (vlax-get x (atd:objprop x)) 0 1)))
                                        (atd:3p->2p (polar p3 (+ halfpi ang) 1))
                                        nil
                                    )
                                )
                            )
                        )
                        ss
                    )
            )
            (initget "Through")
            (setq |ATD_Distance 
                (cond 
                    (   (getdist
                            (strcat
                                "\nSpecify alignment distance or [Through] <" 
                                (if (numberp |ATD_Distance) (rtos |ATD_Distance 2 3) "Through") ">: "
                            )
                        )
                    )
                    (   |ATD_Distance   )
                )
            )
            (if (numberp |ATD_Distance)
                (setq pt (getpoint "\nSpecify base point <through>: "))
            )
            (setvar 'snapang ang)
            (princ (setq msg "\nSpecify alignment point or [Distance] <place>: "))
            (while
                (progn
                    (setq gr  (grread T 15 0)
                          grp (last gr)
                          grv (car gr)
                    )
                    (cond
                        (   (member grv '(5 3))
                            (if
                                (and (numberp |ATD_Distance) (atd:Point-p pt))
                                (if
                                    (not (apply 'LM:Clockwise-p (mapcar 'atd:3p->2p (list pt (polar pt ang 100) grp))))
                                    (setq
                                        movpt (inters
                                                (atd:3p->2p (setq pg (polar pt (+ ang halfpi) |ATD_Distance)))
                                                (atd:3p->2p (polar pg ang 10))
                                                (atd:3p->2p p1)
                                                (atd:3p->2p (polar p1 (- ang halfpi) 10))
                                                nil
                                            )
                                    )
                                    (setq
                                        movpt (inters
                                                (atd:3p->2p (setq pg (polar pt (- ang halfpi) |ATD_Distance)))
                                                (atd:3p->2p (polar pg ang 10))
                                                (atd:3p->2p p1)
                                                (atd:3p->2p (polar p1 (- ang halfpi) 10))
                                                nil
                                            )
                                    )
                                )
                                (setq 
                                    movpt (inters
                                            (atd:3p->2p grp)
                                            (atd:3p->2p (polar grp ang 10))
                                            (atd:3p->2p p1)
                                            (atd:3p->2p (polar p1 (- ang halfpi) 10))
                                            nil
                                        )
                                )
                            )
                            (setq movpt (list (car movpt) (cadr movpt) (caddr p1)))
                            (mapcar
                                '(lambda (x)
                                    (vlax-put (car x) (cadr x) (trans (mapcar '+ (last x) (mapcar '- movpt p1)) 1 0))
                                )
                                alpts
                            )
                            (mapcar '(lambda (x) (redraw x 3)) sse)
                            (if
                                (and (numberp |ATD_Distance) (atd:Point-p pt))
                                (not 
                                    (or
                                        (equal |ATD_Distance 0.0 1e-7)
                                        (= grv 3)
                                    )
                                )
                                (= grv 5)
                            )
                        )
                        (   (= grv 2)
                            (cond
                                (   (member grp '(68 100))	; Keyboard D
                                    (initget "Through")
                                    (setq |ATD_Distance 
                                        (cond 
                                            (   (getdist
                                                    (strcat
                                                        "\nSpecify alignment distance or [Through] <" 
                                                        (if (numberp |ATD_Distance) (rtos |ATD_Distance 2 3) "Through") ">: "
                                                    )
                                                )
                                            )
                                            (   |ATD_Distance   )
                                        )
                                    )
                                    (if (numberp |ATD_Distance)
                                        (setq pt (getpoint "\nSpecify base point <through>: "))
                                    )
                                    (princ msg)
                                )
                                (   (member grp '(80 112))	; Keyboard P
                                    nil
                                )
                                (   T   )
                            )
                        )
                        (   (= grv 25) nil)
                        (   T   )
                    )
                )
            )
        )
    )
    
    (mapcar '(lambda (x) (redraw x 4)) sse)
    (setvar 'snapang snp)
    (if activeundo nil (vla-EndUndoMark adoc))
    (princ)
)

(defun atd:Point-p (arg)
    (and
        (eq (type arg) 'variant)
        (eq (type (setq arg (vlax-variant-value arg))) 'safearray)
        (setq arg (vlax-safearray->list arg))
    )
    (and
        (listp arg)
        (listp (cdr arg))
        (<= 2 (length arg) 3)
        (vl-every 'numberp arg)
    )
)

(defun atd:3p->2p (pt)
    (list (car pt) (cadr pt))
)

(defun atd:objprop (obj)
    (cond
        (   (vlax-property-available-p obj 'insertionpoint)
            (if
                (and
                (vlax-property-available-p obj 'textalignmentpoint)
                (null (equal (vlax-get obj 'textalignmentpoint) '(0 0 0) 1e-8))
                )
                'textalignmentpoint
                'insertionpoint
            )
        )
        (   (vlax-property-available-p obj 'textalignmentpoint) 'textalignmentpoint)
        (   (vlax-property-available-p obj 'textposition) 'textposition)
        (   (vlax-property-available-p obj 'center) 'center)
    )
)

(defun atd:GetDirectionAtCurve (msg / crv det mtx pt vec)
    (while
        (progn
            (setvar "errno" 0)
            (initget "Exit")
            (setq det (nentselp (getpoint msg)))
            (cond
                (   (= (getvar "errno") 7) (princ "\nNothing selected."))
                (   (member det '("Exit" nil)) nil)
                (   (not (wcmatch (cdr (assoc 0 (entget (setq crv (car det))))) "LINE,LWPOLYLINE,ARC,ELLIPSE,CIRCLE,SPLINE"))
                    (princ "\nNo curves detected")
                    )
                (   t
                    (if 
                        (setq pt (trans (cadr det) 1 0) mtx (caddr det))
                        (setq
                            mtx (atd:4x4->3x3 mtx)
                            vec (trans
                                    (mxv 
                                        (car mtx)
                                        (vlax-curve-getfirstderiv crv
                                            (vlax-curve-getparamatpoint crv 
                                                (vlax-curve-getclosestpointto crv 
                                                    (mxv (gc:Inv3x3 (car mtx)) (mapcar '- pt (cadr mtx)))
                                                )
                                            )
                                        )
                                    )
                                    0 1 t
                                )
                        )
                        (setq vec (trans (vlax-curve-getfirstderiv crv (vlax-curve-getparamatpoint crv (vlax-curve-getclosestpointto crv pt))) 0 1 t))
                    )
                    nil
                )
            )
        )
    )
    (if vec (angle '(0.0 0.0 0.0) vec))
)

(defun atd:selset-to-list-vla (ss / rtn)
    (if ss
        (repeat (setq i (sslength ss))
            (setq i (1- i) rtn (cons (vlax-ename->vla-object (ssname ss i)) rtn))
	    )
	)
)

(defun atd:4x4->3x3 (mx)
    (list
        (mapcar
            '(lambda (a)
                (reverse (cdr (reverse a)))
            )
            (reverse (cdr (reverse mx)))
        )
        (mapcar 'last mx)
    )
)

(defun atd:GetCorner (/ ctr rt ss vs zm)    ;; in DCS
    (setq ctr (trans (getvar 'viewctr) 1 2)
          vs  (getvar 'viewsize)
          ss  (getvar 'screensize)
          rt  (/ vs (cadr ss))
          zm  (mapcar '(lambda (a) (abs (* 0.5 rt a))) (append ss '(0.0)))
    )
    (list
        (mapcar '- ctr zm)
        (mapcar '+ ctr zm)
    )
)

;; gc:Inv3x3 
;; Return the inverse transformation matrix (3X3) 
;; 
;; Argument 
;; mat: a 3x3 matrix 

(defun gc:Inv3x3 (mat / a b c d e f g h i det) 
    (mapcar 
        'set 
        '(a b c d e f g h i)
        (mapcar 'float (apply 'append mat))
    )
    (setq det 
        (+ 
            (* a e i) 
            (* b f g)
            (* c d h)
            (- (* c e g))
            (- (* b d i))
            (- (* a f h))
        )
    )
    (if (and (/= 0 det) (setq det (/ 1 det))) 
        (mapcar 
            '(lambda (v) 
                 (mapcar '(lambda (x) (* x det)) v)
             )
            (list 
                (list 
                    (- (* e i) (* f h))
                    (- (* c h) (* b i))
                    (- (* b f) (* c e))
                )
                (list 
                    (- (* f g) (* d i))
                    (- (* a i) (* c g))
                    (- (* c d) (* a f))
                )
                (list 
                    (- (* d h) (* e g))
                    (- (* b g) (* a h))
                    (- (* a e) (* b d))
                )
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Clockwise-p - Lee Mac
;; Returns T if p1,p2,p3 are clockwise oriented

(defun LM:Clockwise-p ( p1 p2 p3 )
    (<
        (* (- (car  p2) (car  p1)) (- (cadr p3) (cadr p1)))
        (* (- (cadr p2) (cadr p1)) (- (car  p3) (car  p1)))
    )
)
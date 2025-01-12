;|  LengthAtMidPoints-P.lsp [command name: LMPP]
To put Text indicating LENGTH(s) of selected object(s) at MIDPOINT(s), with User-specified Prefix.
Draws Text in current Style and on current Layer.
Works with objects in any Coordinate System.
Kent Cooper, 6 March 2023 [adjusted from LengthAtMidPoints.lsp]
|;

(vl-load-com)

(defun C:LMPP ; = Length at Mid-Point with Prefix
  (/ *error* lmp-reset LMPss doc svnames svvals temp n path pathdata pathtype pathextr ucschanged lmp-pt len)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (if ucschanged (command "_.ucs" "_prev"))
      ;; ^ don't go back unless routine reached UCS change but didn't change it back
    (vla-endundomark doc)
    (lmp-reset)
  ); defun - *error*

  (defun lmp-reset ()
    (mapcar 'setvar svnames svvals); reset
    (princ)
  ); defun - lmp-reset

  (prompt "\nTo mark Length(s) at object Midpoint(s),")
  (if
    (setq LMPss (ssget '((0 . "LINE,ARC,CIRCLE,ELLIPSE,*POLYLINE,SPLINE"))))
    (progn ; then
      (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
      (setq
        svnames '(osmode blipmode cmdecho)
        svvals (mapcar 'getvar svnames)
      ); setq
      (if (not *LMPPprefix*) (setq *LMPPprefix* "")); nothing for first-use default
      (setq
        temp
        (getstring T ; allow space(s)
          (strcat
            "\nPrefix or . for none <"
            (if (/= *LMPPprefix* "") *LMPPprefix* "."); prior as default if present
            ">:"
          ); strcat
        ); getstring
        *LMPPprefix* ; global variable to offer as default on subsequent use
        (cond
          ((or (= temp ".") (= temp *LMPPprefix* "")) ""); nothing
          ((and (= temp "") (/= *LMPPprefix* "")) *LMPPprefix*); prior value
          (temp); User input
        ); cond
      ); setq
      (setvar 'cmdecho 0)
      (repeat (setq n (sslength LMPss))
        (setq
          path (ssname LMPss (setq n (1- n)))
          pathdata (entget path)
          pathtype (cdr (assoc 0 pathdata))
          pathtype
            (if (wcmatch pathtype "POLYLINE")
              (strcase (substr (cdr (assoc 100 (reverse pathdata))) 5)); then
                ;; ^ = entity type from second (assoc 100) without "AcDb" prefix;  uses this because (assoc 0)
                ;; value is the same for 2D heavy & 3D Polylines; can set UCS to match former, but not latter
              pathtype ; else - leave alone
            ); if and pathtype
          pathextr (cdr (assoc 210 pathdata))
        ); setq
        (if ; set UCS to match object only under certain circumstances
          (or ; look at entity types other than 3D Polylines and 3D Splines
            (and
              (= pathtype "LINE")
              (not ; unequal Z components at ends, in current CS
                (equal
                  (caddr (trans (cdr (assoc 10 pathdata)) 0 1))
                  (caddr (trans (cdr (assoc 11 pathdata)) 0 1))
                  1e-12
                ); equal
              ); not
            ); and - Line UCS check
            (and
              (wcmatch pathtype "ARC,CIRCLE,ELLIPSE,LWPOLYLINE,2DPOLYLINE")
              (not (equal (trans pathextr 0 1) '(0 0 1) 1e-6)); extrusion direction not = current CS
            ); and - A/C/E/LWP/2dP UCS check
            (and
              (= pathtype "SPLINE")
              (if pathextr (not (equal (trans pathextr 0 1) '(0 0 1) 1e-12)))
                ;; ^ planar [2D] Splines have 210 value; non-planar [3D] do not
            ); and - Spline UCS check
          ); or - need to change UCS
          (progn
            (if (equal pathextr '(0 0 1) 1e-12)
              (command "_.ucs" "_world"); then
              (if (= pathtype "LINE") ; outer else -- set UCS to match object
                (command "_.ucs" (vlax-curve-getStartPoint path) (vlax-curve-getEndPoint path) "")
                  ; then -- sometimes UCS OB on Line does it with Line up Z axis
                (command "_.ucs" "_new" "_object" path); else [other entity types]
              ); if
            ); if
            (setq ucschanged T) ; marker for *error* to reset UCS if routine doesn't get to it
          ); progn
        ); if - UCS match object
        (mapcar 'setvar svnames '(0 0)); Osnap and blips off
        (command
          "_.text" "_justify" "_bc"
          (trans
            (setq lmp-pt ; insertion point
              (vlax-curve-getPointAtDist ; midway along length
                path
                (/
                (setq len (vlax-curve-getDistAtParam path (vlax-curve-getEndParam path))); overall length
                  2
                ); /
              ); getPointAtDist
            ); setq
            0 1 ; WCS to current CS
          )
        ); command ; leave in Text command
        (if (member '(40 . 0.0) (entget (tblobjname "style" (getvar 'textstyle)))) (command ""))
          ; accept current-height default if non-fixed-height or non-annotative Style
        (command ; continue
          (angtos ; rotation -- local direction of path
            (+
              (angle
                '(0 0 0)
                (trans
                  (vlax-curve-getFirstDeriv
                    path
                    (vlax-curve-getParamAtPoint path lmp-pt)
                  ); getFirstDeriv
                  0 1 T; WCS to current CS, as displacement
                ); trans
              ); angle
              (if ; put text on outboard side of Arc/Circle/Ellipse/LWPline arc segment
                (or
                  (wcmatch pathtype "ARC,CIRCLE,ELLIPSE")
                  (and
                    (= pathtype "LWPOLYLINE") ;;;;; what about "heavy" Polyline arc segment?
                    (> ; midway point on arc segment with CCW curvature?
                      (vla-getBulge (vlax-ename->vla-object path) (vlax-curve-getParamAtPoint path lmp-pt))
                      0.0
                    ); >
                  ); and
                ); or
                pi 0 ; then = spin around, else = direction unaltered
              ); if
            ); +
            (getvar 'aunits) 8
          ); angtos
          (strcat *LMPPprefix* (rtos len 2 2)); text content
        ); command
        (if ucschanged (progn (command "_.ucs" "_prev") (setq ucschanged nil)))
          ; eliminate UCS reset in *error* since routine did it already
      ); repeat
      (lmp-reset)
      (vla-endundomark doc)
    ); progn
  ); if
); defun - LMP

(prompt "Type LMP to mark the Lengths of selected objects at their Mid-Points.")
;;  AlignSnapAng.lsp
;;    [commands: AS to Align Snap without changing SnapBase origin,
;;    ASB to Align Snap and set Base at entity origin or selection point]
;;  To Align the Snap Angle (and Base if desired) with a selected entity.
;;  Aligns with:
;;    Direction of Line, Polyline line segment, MultiLine, Xline, Ray, or
;;      picked edge of Viewport/Solid/Wipeout/3DFace;
;;    Tangent to curve (Arc, Circle, Polyline arc segment, Ellipse, Spline) at selection point;
;;    Rotation angle of Text or Mtext, or of first definition line of Hatch pattern.
;;  In ASB, also sets SnapBase System Variable to:
;;    Insertion point of Text/Mtext/Block/Xref/Attdef;
;;    Start point of Line/Leader/MultiLine/Ray/Polyline line segment;
;;    Center point of Circle/Arc/Ellipse/Polyline arc segment;
;;    Selection point on Spline.
;;  Does not [yet] align with nested entities.
;;  Kent Cooper, last edited 20 August 2013
;
(defun ALSN (/ apert esel path)
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'aperture apert)
  ); defun
  (setq apert (getvar 'aperture))
  (setvar 'aperture (getvar 'pickbox)); less likely to snap to wrong object
  (while
    (not
      (and
        (setq esel (entsel "\nSelect object to Align Snap with: "))
        (setq
          edata (entget (car esel))
          etype (cdr (assoc 0 edata))
        ); setq
        (wcmatch etype "*LINE,ARC,CIRCLE,ELLIPSE,LEADER,HATCH,TEXT,MTEXT,INSERT,VIEWPORT,TRACE,SOLID,WIPEOUT,3DFACE,ATTDEF,AEC_WALL")
      ); and
    ); not
    (prompt "\nNothing selected, or cannot Align Snap with that object --")
  ); while
  (cond
    ((= etype "HATCH") (setvar 'snapang (cdr (assoc 52 edata))))
    ((wcmatch etype "*TEXT,INSERT,ATTDEF")
      (setvar 'snapang (cdr (assoc 50 edata)))
    ); text/mtext/block/xref/attribute types
    ((wcmatch etype "MLINE,TRACE,SOLID,VIEWPORT,WIPEOUT,3DFACE,AEC_WALL")
      (setq picknear (osnap (cadr esel) "nea"))
      (setvar 'snapang (angle picknear (osnap picknear "end")))
    ); non-"curve" types with straight edges that can be Osnapped to
    (T; for everything else
      (setq
        path (car esel)
        picknear (osnap (cadr esel) "nea")
      ); setq
      (setvar 'snapang
        (angle
          '(0 0 0)
          (vlax-curve-getFirstDeriv path (vlax-curve-getParamAtPoint path picknear))
        ); angle
      ); setvar
    ); linear-curvilinear entity types
  ); cond - type of entity
  (setvar 'aperture apert)
); defun - ALSN
;
(defun XY (xyz)
  (list (car xyz) (cadr xyz))
); defun - XY
;
(defun C:AS (/ *error* edata etype path picknear); = Align Snap
  (ALSN)
  (princ)
); defun - AS
;
(defun C:ASB (/ *error* edata etype picknear); = Align Snap & set snapBase location
  (ALSN)
  (setvar 'snapbase
    (cond
      ((= etype "HATCH")
        (load HatchOriginalBase.lsp)
        (xy (HatchBase))
      ); Hatch condition
      ((= etype "TEXT"); insertion point
        (if (= (cdr (assoc 72 edata)) (cdr (assoc 73 edata)) 0); left justified
          (xy (cdr (assoc 10 edata))); then
          (xy (cdr (assoc 11 edata))); else
        ); if
      ); Text condition
      ((wcmatch etype "LINE,CIRCLE,ARC,ELLIPSE,MTEXT,INSERT,ATTDEF")
        (xy (cdr (assoc 10 edata)))
      ); Mtext/Insert condition
      (T (xy picknear)); everything else ;;;;; this instead of (assoc 10) for Line/Circle/Arc/Ellipse, too?
    ); cond
  ); setvar
  (princ)
); defun - ASB
;
(vl-load-com)
(prompt "\nType AS to Align Snap with object, or\n  ASB to Align and set Snap Base on object.")

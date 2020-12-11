(setvar "MTEXTCOLUMN" 0)
;(COMMAND "MTEXTAUTOSTACK" "OFF")(COMMAND)
(setvar "tstacksize" 100)
(setvar "tstackalign" 1)
(setvar "mirrtext" 0)

(defun c:zerodel ()
  (setq ssline (ssget '((0 . "LINE"))))
  (setq ssdelete (ssadd))
  (setq lstline (sslist ssline))
  (setq x 0)
  (repeat (length lstline)
    (setq endpts (endptget (nth x lstline)))
    (setq linelength (sqrt (+ (expt (- (nth 0 (nth 1 endpts)) (nth 0 (nth 0 endpts))) 2) (expt (- (nth 1 (nth 1 endpts)) (nth 1 (nth 0 endpts))) 2) (expt (- (nth 2 (nth 1 endpts)) (nth 2 (nth 0 endpts))) 2))))
	  (if (< linelength 0.001)
	    (ssadd (nth x lstline) ssdelete)
	  );if
    (setq x (1+ x))
  );repeat
  (command-s "erase" ssdelete "")
);defun

(defun c:wavemoment ( / )
  (princ)
  (varget)
  (if (not wavepl)
    (progn
      (setq wavepl (car(entsel "Select Wave Polyline: ")))
      (terpri)
      (setq dockpl (car(entsel "Select Dock Polyline: ")))
      (terpri)
      (setq docklength (getreal "Enter Length of Dock: "))
      (terpri)
      (setq areareq (getreal "Enter Area Required: "))
      (terpri)
      (setq internalpt (getpoint "Select Initial Internal Point: "))
      (terpri)
      (setq amidpt (getpoint "Select Amidships Point: "))
      (terpri)
    );progn
  );if
  (setvar "nomutt" 1)
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (command ".boundary" internalpt "")(command)
  (setq boundpts (ptget(entlast)))
  (setq boundarea (abs(area boundpts)))
  (command "erase" (entlast) "")
  (setq change (/ (- boundarea areareq) docklength))
  (while (> (abs change) (/ 1.0 docklength))
    (command "move" dockpl "" (list 0 0 0) (list 0 change 0))(command)
    (command ".boundary" internalpt "")(command)
    (setq boundpts (ptget(entlast)))
    (setq boundarea (abs(area boundpts)))
    (command "erase" (entlast) "")
    (if (< (/ (- boundarea areareq) change) 0.0)
      (setq change (* -0.5 change))
    );if
  );while
  (setvar "nomutt" 0)
  (terpri)
  (princ (strcat "Final Area is " (rtos boundarea 2 2)))
  (terpri)
  (setq arm (- (nth 0 amidpt) (nth 0 (centroid boundpts boundarea))))
  (setq selfmoment (* docklength 0.25 areareq))
  (princ (strcat "Final Moment is " (rtos (- (* boundarea arm) selfmoment) 2 2)))
  (terpri)
  (if (= (strcase(getstring "Clear Initial Setup?: ")) "Y")
    (progn
      (setq wavepl      nil
            dockpl      nil 
            areareq     nil
            internalpt  nil
            docklength  nil
            amidpt      nil
      );setq
    );progn
  );if
  (princ)
  (varres)
);defun

(defun c:lenall ( / totallength output ssblk ent obj x)
  (princ)
  (varget)
  (setvar "cmdecho" 0)
  (vl-load-com)
  (setvar "cmdecho" 1)
  (prompt "Select Lines, Polylines, or Arcs:")
  (setvar "nomutt" 1)
  (setq ssblk (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (0 . "LINE") (0 . "ARC") (-4 . "OR>"))))
  
  (terpri)
  (setq x 0)
  (setq totallength 0)
  (repeat (sslength ssblk)
    (setq ent (ssname ssblk x))
    (setq obj (vlax-ename->vla-object ent))
    (if (= "AcDbArc" (vlax-get-property obj "ObjectName"))
      (setq totallength (+ totallength (vlax-get-property obj "ArcLength")))
      (setq totallength (+ totallength (vlax-get-property obj "Length")))
    );if
    (vlax-release-object obj)
    (setq x (+ x 1))
  );repeat
  (terpri)
  (setq output (vl-string-right-trim "'" (rtos totallength 2)))
  
  (varres)
  (setvar "nomutt" 0)
  (terpri)
  (princ (strcat "Total Length of Lines " output))
  (princ)
);defun

;(defun c:rotinc ()
;  (setq go 1)
;  (setq x (getreal))
;  (setq center (getpoint))
;  (setq increment x)
;  (while (= go 1)
;    (command "rotate" (entsel) "" center increment)(command)v;li
;    (setq increment (+ increment x))
;  );while
;);defun

(defun ptget (ent / ent obj n corrl pts otype)
  (setq obj (vlax-ename->vla-object ent))
  (setq otype (vlax-get-property obj "ObjectName"))
  (setq n 0)
  (setq pts '(spooky))
  (if (= otype "AcDb3dPolyline")
    (progn
    (setq corrl (vlax-get-property obj "Coordinates"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (repeat (/ (length corrl) 3)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl)))))
        (setq n (+ n 3))
      );repeat
    );progn
  );if
  (if (= otype "AcDbPolyline")
    (progn
      (setq corrl (vlax-get-property obj "Coordinates"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (repeat (/ (length corrl) 2)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl)))))
        (setq n (+ n 2))
      );repeat
    );progn
  );if
  (setq pts (cdr pts)) 
  (if (not (= (nth (- (length pts) 1) corrl) (nth 0 pts)))
    (progn
      (setq pts (append pts (list (nth 0 pts))))
    );progn
  );if
  (vlax-release-object obj)
  (cond
    ((= pts) pts)
    (t nil)
  );cond
);defun ptget

(defun endptget (ent / ent obj n corrl pts otype)
  (setq obj (vlax-ename->vla-object ent))
  (setq otype (vlax-get-property obj "ObjectName"))
  (setq n 0)
  (setq pts '(spooky))
  (if (= otype "AcDb3dPolyline")
    (progn
      (setq corrl (vlax-get-property obj "Coordinates"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (repeat (/ (length corrl) 3)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl)))))
        (setq n (+ n 3))
      );repeat
    );progn
  );if
  (if (= otype "AcDbPolyline")
    (progn
      (setq corrl (vlax-get-property obj "Coordinates"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (repeat (/ (length corrl) 2)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl)))))
        (setq n (+ n 2))
      );repeat
    );progn
  );if
  (if (or (= otype "AcDbLine") (= otype "AcDbEllipse"))
    (progn
      (setq corrl (vlax-get-property obj "StartPoint"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (setq corrl (append corrl (vlax-safearray->list (vlax-variant-value (vlax-get-property obj "EndPoint")))))
      (repeat (/ (length corrl) 3)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl) (nth (+ n 2) corrl)))))
        (setq n (+ n 3))
      )
    );progn
  );if
  
  (setq pts (cdr pts)) 
  
  (vlax-release-object obj)
  (cond
    ((= pts) pts)
    (t nil)
  );cond
);defun ptget

(defun 3dptget (ent / ent obj n corrl pts otype elev)
  (setq obj (vlax-ename->vla-object ent))
  (setq corrl (vlax-get-property obj "Coordinates"))
  (setq otype (vlax-get-property obj "ObjectName"))
  (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
  (setq n 0)
  (setq pts '(spooky))
  (if (= otype "AcDb3dPolyline")
    (progn
      (repeat (/ (length corrl) 3)
      ;(setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl) (nth (+ n 2) corrl)))))
	      (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl) (nth (+ n 2) corrl)))))
        (setq n (+ n 3))
      );repeat
    );progn
    
  );if
  (if (= otype "AcDbPolyline")
    (progn
      (setq corrl (vlax-get-property obj "Coordinates"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (setq elev (vlax-get-property obj "Elevation"))
      (repeat (/ (length corrl) 2)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl) elev))))
        (setq n (+ n 2))
      );repeat
    );progn
  );if
  (if (= otype "AcDbLine")
    (progn
      (setq corrl (vlax-get-property obj "StartPoint"))
      (setq corrl (vlax-safearray->list (vlax-variant-value corrl)))
      (setq corrl (append corrl (vlax-safearray->list (vlax-variant-value (vlax-get-property obj "EndPoint")))))
      (repeat (/ (length corrl) 3)
        (setq pts (append pts (list(list (nth n corrl) (nth (+ n 1) corrl) (nth (+ n 2) corrl)))))
        (setq n (+ n 3))
      )
    );progn
  );if
  (setq pts (cdr pts)) 
  (if (not (= (nth (- (length pts) 1) corrl) (nth 0 pts)))
    (progn
      (setq pts (append pts (list (nth 0 pts))))
    );progn
  );if
  (vlax-release-object obj)
  (cond
    ((= pts) pts)
    (t nil)
  );cond
);defun ptget

(defun arcptget (ent pts / rlst x lstexpanded r pt1 pt2 dist theta thetaR thetaRc ptR rad center start end n angchg segments)
  (setq rlst (vl-remove-if '(lambda (q)(= nil q)) (mapcar '(lambda (s) (if (= (nth 0 s) 42) (cdr s))) (entget ent))))
  (setq x 0)
  (setq lstexpanded (list(nth 0 pts)))
  (repeat (- (length pts) 1)
    (setq pt1 (nth x pts))
    (setq pt2 (nth (+ x 1) pts))
    (setq r (nth x rlst))
    
    (if (> (abs r) 0.0)
      (progn
        (setq dist (ptdist pt1 pt2))
        (setq theta (fullangle pt1 pt2)) ;angle of the line between the two points. Following lines find endpoint of perpendicular bisector
        (setq ptR (list (+ (nth 0 pt1) (* (/ dist 2) (cos theta)) (* (/ dist 2) r (sin theta))) (+ (nth 1 pt1) (* (/ dist 2) (sin theta)) (* -1 (/ dist 2) r (cos theta)))))
        (setq thetaR (fullangle (list (+ (nth 0 pt1) (* (/ dist 2) (cos theta))) (+ (nth 1 pt1) (* (/ dist 2) (sin theta)))) ptR))
        (setq thetaRc (fullangle pt1 ptR))
        (if (> (abs(- thetaR thetaRc)) pi)
          (setq thetaR (+ thetaR (* 2 pi)))
        );if
        (setq rad (/ (ptdist pt1 ptR) (* 2 (cos (abs (- thetaR thetaRc)))))) ;Radius of hte circle
        (setq center (list (- (nth 0 ptr) (* rad (cos thetaR))) (- (nth 1 ptr) (* rad (sin thetaR)))))

        (setq start (fullangle center pt1))
        (setq end (fullangle center pt2))
        (cond
          ((and (< r 0) (< end start))
           (setq angchg (- end start))
          );cond1
          ((and (< r 0) (> end start))
           (setq angchg (- (- end (* 2 pi)) start))
          );cond2
          ((and (> r 0) (> end start))
           (setq angchg (- end start))
          );cond3
          ((and (> r 0) (< end start))
           (setq angchg (- (+ end (* 2 pi)) start))
          );cond4
        );cond
        
        (setq segments (fix (* 32 (/ (abs angchg) pi))))
        (setq n 1)
        (setq lstln '("list of arc points"))
        (repeat (- segments 1)
          (setq lstln (append lstln (list(list (+ (* rad (cos (+ start (/ (* n angchg) segments)))) (nth 0 center)) (+ (* rad (sin (+ start (/ (* n angchg) segments)))) (nth 1 center))))))
          (setq n (1+ n))
        );repeat
        (setq lstln (append lstln (list (list (+ (* rad (cos end)) (nth 0 center)) (+ (* rad (sin end)) (nth 1 center))))))
        (setq lstln (cdr lstln))
      );progn
      (setq lstln (list pt2))
    );if
    
    (setq lstexpanded (append lstexpanded lstln))
    
    (setq x (1+ x))
  );repeat
  
  ;(setq oldosmode (getvar "osmode"))
  ;(setvar "osmode" 0)
  ;(setq n 0)
  ;(repeat (- (length lstexpanded) 1)
  ;  (apply 'command (append '("line") (list (nth n lstexpanded) (nth (+ n 1) lstexpanded)) '("")))
  ;  (setq n (1+ n))
  ;);repeat
  ;(setvar "osmode" oldosmode)
  
  (cond
    ((= lstexpanded) lstexpanded)
    (t nil)
  );cond
);defun

(defun ptdist (pt1 pt2 / x pt1 pt2 dist)
  (setq x 0)
  (setq dist 0)
  (repeat (min (length pt1) (length pt2))
    (setq dist (+ dist (expt (- (nth x pt2) (nth x pt1)) 2)))
    (setq x (1+ x))
  );repeat
  (setq dist (sqrt dist))
  (cond
    ((= dist) dist)
    (t nil)
  );cond
);defun

(defun area (pts / ptarea x1 x2 y1 y2 n pts)
  (setq ptarea 0)
  (setq n 0)
  (repeat (- (length pts) 1)
    (setq x1 (nth 0 (nth n pts))
          x2 (nth 0 (nth (+ n 1) pts))
          y1 (nth 1 (nth n pts))
          y2 (nth 1 (nth (+ n 1) pts)))
    (setq ptarea (+ ptarea (- (* x1 y2) (* x2 y1))))
    (setq n (+ n 1))
  );repeat
  (setq ptarea (/ ptarea 2))
  (cond
    ((= ptarea) ptarea)
    (t nil)
  );cond
);defun area

(defun centroid (pts ptarea / pts ptarea center x1 x2 y1 y2 n pts cx cy)
  (setq cx 0)
  (setq cy 0)
  (setq n 0)
  (repeat (- (length pts) 1)
    (setq x1 (nth 0 (nth n pts))
          x2 (nth 0 (nth (+ n 1) pts))
          y1 (nth 1 (nth n pts))
          y2 (nth 1 (nth (+ n 1) pts)))
    (setq cx (+ cx (* (+ x1 x2) (- (* x1 y2) (* x2 y1)))))
    (setq cy (+ cy (* (+ y1 y2) (- (* x1 y2) (* x2 y1)))))
    (setq n (+ n 1))
  );repeat
  (setq cx (/ cx (* 6 ptarea)))
  (setq cy (/ cy (* 6 ptarea)))
  (setq center (list cx cy))
  (cond
    ((= center) center)
    (t nil)
  );cond
);defun centroid

(defun inertia (pts center / x ptinertia xi xii yi yii center pts)
  (setq x 0)
  (setq ptinertia 0)
  (repeat (- (length pts) 1)
    (setq 	xi (nth 0 (nth x pts))
    		yi (- (nth 1 (nth x pts)) (nth 1 center))
		    xii (nth 0 (nth (+ 1 x) pts))
    		yii (- (nth 1 (nth (+ 1 x) pts)) (nth 1 center)))
    (setq ptinertia (+ ptinertia (* (+ (expt yi 2) (* yi yii) (expt yii 2)) (- (* xi yii) (* xii yi)))))
    (setq x (1+ x))
  );repeat
  (setq ptinertia (/ ptinertia 12))
  (cond
    ((= ptinertia) ptinertia)
    (t nil)
  );cond
);defun inertia

(defun sslist (sset / x lstent sset) ; Create a list of entnames instead of selection set
  (setq x 0)
  (setq lstent '("ListOfEntNames"))
  (repeat (sslength sset)
    (setq lstent (append lstent (list (ssname sset x))))
    (setq x (1+ x))
  );repeat
  (setq lstent (cdr lstent))
  (cond
    ((= lstent) lstent)
    (t nil)
  );cond
);defun

(defun fullangle (pt1 pt2 / pt1 pt2) ;Find angle of pt2 around pt1 with respect to +x-axis
  (setq ptangle (atan (- (nth 1 pt2) (nth 1 pt1)) (- (nth 0 pt2) (nth 0 pt1))))
  (if (< ptangle 0)
    (setq ptangle (+ ptangle (* 2 pi)))
  );if
  (cond
    ((= ptangle) ptangle)
    (t nil)
  );cond
);defun

(defun intersect (lstln / x e obj1 lstln lstintent)
  (setq x 0)
  (setq lstint '("list of intercepts"))
  (repeat (length lstln)
    (setq obj1 (vlax-ename->vla-object (nth x lstln)))
    (setq e x)
    (repeat (length lstln)
      (if (not (= x e))
	      (progn
	        (setq lstintentln (vlax-invoke obj1 'IntersectWith (vlax-ename->vla-object (nth e lstln)) acExtendNone))
	        (setq lstintent (append lstintent (list lstintentln)))
	      );progn
      );if
      (setq e (1+ e))
    );repeat
    (setq lstintent (cdr lstintent))
    
    ;need to remove duplicate intersections from the list for a single entity
    
    (setq e 0)
    (while (< e (length lstintent))
      (setq lstintent (append (list(nth e lstintent)) (vl-remove-if '(lambda (q)(equal (nth e lstintent) q 0.000001)) lstintent)))
      (setq e (1+ e))
    );while

    ;also remove nil entries in the intersection list
    
    (setq lstintent (vl-remove-if '(lambda (q)(= nil q)) lstintent))

    (setq lstint (append lstint lstintent))
    (setq x (1+ x))
  );repeat
  (setq lstint (cdr lstint))

  (setq e 0)
  (while (< e (length lstint))
    (setq lstint (append (list(nth e lstint)) (vl-remove-if '(lambda (q)(equal (nth e lstint) q 0.000001)) lstint)))
    (setq e (1+ e))
  );while

  ;also remove nil entries in the intersection list
    
  (setq lstint (vl-remove-if '(lambda (q)(= nil q)) lstint))

  (cond
    ((= lstint) lstint)
    (t nil)
  )
);defun

(defun intersingle (ent1 lstln / x ent1 obj1 lstintentln lstln lstint)
  (setq x 0)
  (setq lstint '("list of intercepts"))
 
  (setq obj1 (vlax-ename->vla-object ent1))
    
  (repeat (length lstln) 
    (setq lstintentln (vlax-invoke obj1 'IntersectWith (vlax-ename->vla-object (nth x lstln)) acExtendNone))
    (setq lstint (append lstint (list lstintentln)))
    (setq x (1+ x))
  );repeat
  (setq lstint (cdr lstint))
    
  ;need to remove duplicate intersections from the list for a single entity
  (setq x 0)
  (while (< x (length lstint))
    (setq lstint (append (list(nth x lstint)) (vl-remove-if '(lambda (q)(equal (nth x lstint) q 0.000001)) lstint)))
    (setq x (1+ x))
  );while

  ;also remove nil entries in the intersection list
  (setq lstint (vl-remove-if '(lambda (q)(= nil q)) lstint))

  (cond
    ((= lstint) lstint)
    (t nil)
  )
);defun

(defun c:t2mt (/ txtss e elist x oldcontent oldlocation obj oldrotation int_pt)
	(varget)
	(vl-load-com)
  (if (ssget "_I" '((0 . "TEXT")))
    (progn 
      (setq txtss (ssget "_I" '((0 . "TEXT"))))
    );progn
    (setq txtss (ssget '((0 . "TEXT"))))
  );if
	
	(if (not (= txtss nil))
		(progn
			(setq x 0)
			;(setvar "clayer" "CEC TEXT")
			(command ".undo" "be")
			(setvar "cmdecho" 0)
			(setvar "osmode" 0)
			(repeat (sslength txtss)
				(setq e (ssname txtss x))
				(setq elist (entget e))
				(setq oldcontent (assoc 1 elist))
				(setq oldlocation (assoc 10 elist))
				(setq oldrotation (cdr(assoc 50 elist)))
        (setq oldheight (cdr (assoc 40 elist)))
        (setq oldstyle (cdr(assoc 7 elist)))
        (setq oldlayer (cdr(assoc 8 elist)))
        (setvar "clayer" oldlayer)
        (command ".style" oldstyle "" oldheight "" "" "" "")(command)
				(command "mtext" (cdr oldlocation) (list (+ (nth 0 (cdr oldlocation)) 0.0001) (+ (nth 1 (cdr oldlocation)) 0.0001)) (cdr oldcontent) "")
				(setq ent (entlast))
				(setq obj (vlax-ename->vla-object ent))
				(setq int_pt (vlax-get-property obj 'InsertionPoint))
				(vlax-put-property obj 'AttachmentPoint 7)
				(vlax-put-property obj 'InsertionPoint int_pt)
				(vlax-put-property obj 'Rotation oldrotation)
				(vlax-release-object obj)
				(setq x (+ x 1))
			);repeat
			(command "erase" txtss "")
			(command ".undo" "e")
		);progn
	  	(progn
		(terpri)
		(princ "No Text Objects Selected.")
		);progn
	);if
	(varres)
);defun

(defun c:cord ( / ent obj coorl) ; List VL properties of an object
  (setq ent (entsel "\nSelect Object: "))
  (setq obj (vlax-ename->vla-object (car ent)))
  (setq corrl (vlax-dump-object obj))
  ;(setq corrl(vlax-safearray->list (vlax-variant-value corrl)))
  ;(princ corrl)
  (vlax-release-object obj)
  (princ)
);defun

(defun c:vcg ( / ent obj coorl) ; List out the volumes and VCGs of multiple solids
  (setq entlist (sslist (ssget '((0 . "3dsolid")))))
  (setq n 0)
  (terpri)
  (princ "Volume       VCG")
  (terpri)
  (repeat (length entlist)
    (setq obj (vlax-ename->vla-object (nth n entlist)))
    (princ (rtos (vlax-get-property obj "Volume") 2))
    (princ (strcat "   " (rtos (nth 2 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj "Centroid")))) 2)))
    (terpri)
    (vlax-release-object obj)
    (setq n (1+ n))
  );repeat
  (setq entlist nil)
  ;(princ corrl)
  
  (princ)
);defun

(defun c:lsln ( / entlist) ; List out the lengths of multiple lines
  (setq entlist (sslist(ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "POLYLINE") (0 . "LINE") (-4 . "OR>")))))
  (setq n 0)
  (terpri)
  (princ "Length ")
  (terpri)
  (repeat (length entlist)
    (setq obj (vlax-ename->vla-object (nth n entlist)))
    (princ (rtos (vlax-get-property obj "Length") 2))
    (terpri)
    (vlax-release-object obj)
    (setq n (1+ n))
  );repeat
  (setq entlist nil)
  (princ)
);defun

(defun c:cgl ( / ent obj coorl) ; Make a line from (0,0,0) to the Centroid of a Solid 
  (varget)
  (setq entlist (sslist (ssget '((0 . "3dsolid")))))
  (setq n 0)
  (terpri)
  ;(princ "Volume       VCG")
  (terpri)
  (setvar "osmode" 0)
  (repeat (length entlist)
    (setq obj (vlax-ename->vla-object (nth n entlist)))
    (command "line" (list 0 0 0) (vlax-safearray->list (vlax-variant-value (vlax-get-property obj "Centroid")))) (command)
    (vlax-release-object obj)
    (setq n (1+ n))
  );repeat
  (setq entlist nil)
  (varres)
  ;(princ corrl)
  (princ)
);defun

(defun c:ead () ; Quicker Leader
  (varget)
  (setvar "clayer" "G-ANNO-NOTE")
  (command "leader" pause pause "" (getstring T "Enter Annotation Text:") "")
  (varres)
);defun ead

(defun c:IPS () ; Insert Imperial Paperspace Library
  (varget)
  (command "insert" "L:\\Heger Personnel Folders\\LMR\\Imperial Paper Space Library.dwg" (list 6 24) 1 1 "")(command)
  (command "explode" (entlast))(command)
  (varres)
  (princ)
);defun

(defun c:MPS () ; Insert Metric Paperspace Library
  (varget)
  (command "insert" "L:\\Heger Personnel Folders\\LMR\\Metric Paper Space Library.dwg" (list 150 600) 1 1 "")(command)
  (command "explode" (entlast))(command)
  (varres)
  (princ)
);defun

(defun c:CBL () ; Sets selected entities colors to "by Layer"
  (varget)
  (setq ss (ssget))
  (setq ss (sslist ss))
  (setq x 0)
  (repeat (length ss)
    (setq e (nth x ss))
    (setq elist (entget e))
    (setq destcolor 256)
    (setq elist (subst (cons 62 destcolor) (assoc 62 elist) elist))
    (entmod elist)
    (setq x (+ x 1))
  );repeat
  (varres)
  (princ)
);defun

(defun emod (ent dxfcode newvalue / ent elist dxfcode newvalue);Helper function to mod entities, similar to vlax-put-property. Input variables are the entname, the dxfcode to change, and the new value
  (setq elist (entget ent))
  (setq elist (subst (cons dxfcode newvalue) (assoc dxfcode elist) elist))
  (entmod elist)
);defun

(defun omod (ent propname newvalue / ent obj propname newvalue);Helper function to mod entities, shorthand for vlax-put-property.
  (setq obj (vlax-ename->vla-object ent))
  (vlax-put-property obj propname newvalue)
);defun

(defun oget (ent propname / ent obj propname);Helper function to mod entities, shorthand for vlax-put-property.
  (setq obj (vlax-ename->vla-object ent))
  (vlax-get-property obj propname)
);defun

(defun c:xrfix ()
  (setq acad (vlax-get-Acad-Object))
  (setq doc (vla-get-activedocument acad))
  (setq olddwgname (vlax-get-property doc "FullName"))
  (setq documents (vlax-get-property acad "Documents"))
  (setq dwgname (getfiled "Get File" (getvar "dwgprefix") "dwg" 2))
  ;(setq dwgname (getfiled "Geometry File" (getvar "dwgprefix") "dwg" 2))
  ;(setq dwg-collection (vl-catch-all-apply 'vla-open (list (vlax-get-property doc "Application") filepath)))
  ;(setq startpath (getvar "dwgprefix"))
  (setq v-dwgname (vla-open documents dwgname))
  (vla-put-ActiveDocument acad v-dwgname)
  (command "_.delay" 1000)
  (setq lstxref (sslist(ssget "_X" '((0 . "INSERT")))))
  (setq lstpathraw (mapcar '(lambda (s) (vl-catch-all-apply 'vlax-get-property (list (vlax-ename->vla-object s) "Path"))) lstxref))
  (setq x 0)
  (repeat (length lstpathraw)
    (if (not (vl-catch-all-error-p (nth x lstpathraw)))
      (progn
        (setq path (nth x lstpathraw))
        (if (= (substr path 1 1) "X")
          (omod (nth x lstxref) "Path" (strcat ".\\0-Xrefs\\HDD\\" path))
        );if
        (if (or (= (substr path 1 1) "D") (= (substr path 1 1) "E") (= (substr path 1 1) "M"))
          (omod (nth x lstxref) "Path" (strcat ".\\0-Xrefs\\Stantec\\" path))
        );if
        (if (or (= (substr path 1 1) "P") (= (substr path 1 1) "p"))
          (omod (nth x lstxref) "Path" (strcat ".\\0-Xrefs\\Title Block\\" path))
        );if
      );progn
    );if
    (setq x (1+ x))
  )
  ;(command "_.delay" 100)
  (command "regenall")(command)
  (command "qsave")(command)
  (command "_CLOSE" "Y")(command)
  (command "regenall")(command)
  (vla-put-ActiveDocument acad (vla-open documents olddwgname))
)

(defun contents (ent / ent txtstring)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (setq txtstring "ERROR: Not text, mtext, or mleader")
  (if (or (= etype "MTEXT") (= etype "TEXT"))
    (setq txtstring (cdr (assoc 1 elist)))
    (if (= etype "MULTILEADER")
      (setq txtstring (cdr (assoc 304 elist)))
    );if
  );if
  (cond
    ((= txtstring) txtstring)
    (t nil)
  );cond
);defun

(defun varget ()
  (vl-load-com)
  (defun *error* ( msg )
    (princ "error: ")
    (princ msg)
    (varres)
  );defun
  (setq oldcmdecho (getvar "cmdecho"))
  (setq old3dosmode (getvar "3dosmode"))
  (setq oldosmode (getvar "osmode"))
  (command-s "undo" "m")(command)
  (princ)
);defun varget

(defun varres ( / )
  (setvar "cmdecho" oldcmdecho)
  (setvar "osmode" oldosmode)
  (command-s "undo" "e" "")
  (setvar "nomutt" 0)
  (princ)
);defun varres
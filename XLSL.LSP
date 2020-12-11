(defun c:xlsl ()
  (varget)
  (setq ssl (ssget  '((0 . "LINE")) ))
  (setq ssdb (ssget  '((-4 . "<OR") (0 . "CIRCLE") (0 . "ARC") (-4 . "OR>")) ))
  (setq ss3dpl (ssget  '((0 . "POLYLINE")))) ;Selection of set of all the design vectors, planes and groups. Vectors will have 2 verticies, planes 3, groups >3
  (terpri)
  (setq nlc (getint "Enter the Number of Load Combinations:"))
  (setq x 0)
  (setq lstlc '("This is a list of Load combinations"))
  (repeat nlc
    (setq ssf (ssget  '((0 . "MULTILEADER"))))
    (setq lstlc (append lstlc (list(sslist ssf))))
    (setq x (1+ x))
  );repeat
  (setq lstlc (cdr lstlc))
  (setq excel-app (vlax-get-or-create-object "excel.application"))
  (setq filepath (getfiled "Geometry File" (getvar "dwgprefix") "xlsm" 2))
  (setq wb-collection (vl-catch-all-apply 'vla-open (list (vlax-get-property excel-app "workbooks") filepath)))
  (setq Sheets (vl-catch-all-apply 'vlax-get-property (list wb-collection 'sheets)))
  (vlax-put-property excel-app 'calculation -4135)
  (setq x 1)
  (repeat (vlax-get-property sheets "count")
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Constraints")
      (setq Concells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Members")
      (setq memcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Loads")
      (setq Loadcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Properties")
      (setq Propcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Parameters")
      (setq Paramcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Design Vectors")
      (setq dvcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Design Planes")
      (setq dpcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "Design Groups")
      (setq dgcells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" x) "name") "CrossSection")
      (setq cscells (vlax-get (vlax-get-property sheets "item" x) "cells"))
    );if
    (setq x (1+ x))
  )

  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Members") 'RANGE "A:F") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Constraints") 'RANGE "A:F") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Loads") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Properties") 'RANGE "A:D") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Design Vectors") 'RANGE "A:F") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Design Planes") 'RANGE "A:F") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Design Groups") 'RANGE "A:XFD") 'clearcontents)
  
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Topology") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "CrossSection") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Deformation") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Axial") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Moment") 'RANGE "A:XFD") 'clearcontents)
  (vlax-invoke-method (vlax-get-property (vlax-get-property sheets "item" "Gradient") 'RANGE "A:B") 'clearcontents)
  
  (setq nsd (atoi(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property paramcells "item" 3 2)) 'Value) 8))))
  (setq ndf (atoi(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property paramcells "item" 4 2)) 'Value) 8))))
  
  (setq x 1)
  (repeat (sslength ssl)
    (setq l (ssname ssl (- x 1)))
    (setq start (assoc 10 (entget l)))
    (setq end (assoc 11 (entget l)))
    (setq n 1)
    (repeat nsd
      (vlax-put-property memcells "item" x n (rtos (nth n start) 2 4))
      (vlax-put-property memcells "item" x (+ n nsd) (rtos (nth n end) 2 4))
      (setq n (+ n 1))
    )
  
    (vlax-put-property propcells "item" x 3 "0")
    (if (= (cdr (assoc 8 (entget l))) "TENSION")
      (vlax-put-property propcells "item" x 3 "1")
    );if
    (if (= (cdr (assoc 8 (entget l))) "COMPRESSION")
      (vlax-put-property propcells "item" x 3 "-1")
    );if
	
    (if (> (cdr (assoc 39 (entget l))) 0)
      (vlax-put-property propcells "item" x 1 (rtos (cdr (assoc 39 (entget l))) 2 4))
      (vlax-put-property propcells "item" x 1 "1")
    )
    
    (setq x (1+ x))
  );repeat
	 
  (setq x 1)
  (repeat (sslength ssdb)
    (setq db (entget (ssname ssdb (- x 1))))
    (setq dbent (ssname ssdb (- x 1)))
    (setq c (vlax-safearray->list (vlax-variant-value (oget dbent "CENTER"))))
    (setq nvec (vlax-safearray->list (vlax-variant-value (oget dbent "NORMAL"))))
    (setq n 0)
    (setq lstBoundary '("List of Boundary Conditions"))
    (repeat nsd
      (setq lstBoundary (append lstBoundary (list(abs (- (abs (nth n nvec)) 1)))))
      (setq n (+ n 1))
    )
    (setq lstBoundary (cdr lstBoundary))
    (setq n 0)
    (repeat nsd
      (vlax-put-property concells "item" x (+ n 1) (rtos (nth n c) 2 4))
      (setq n (+ 1 n))
    );repeat
    (if (= "CIRCLE" (cdr(assoc 0 db)))
      (progn
        (setq n 0)
        (repeat nsd
          (vlax-put-property concells "item" x (+ n nsd 1) (rtos (nth n lstBoundary) 2 0))
          (setq n (+ 1 n))
        );repeat
      );progn
      (progn
        (if (or (= (cdr (assoc 50 db)) 0) (= (cdr (assoc 50 db)) pi))
          (progn
            (vlax-put-property concells "item" x (+ 1 nsd) "0")
            (vlax-put-property concells "item" x (+ 2 nsd) "1")
            (if (= ndf 3)
              (vlax-put-property concells "item" x (+ 3 nsd) "0")
            )
          );progn
          (progn
            (vlax-put-property concells "item" x (+ 1 nsd) "1")
            (vlax-put-property concells "item" x (+ 2 nsd) "0")
            (if (= ndf 3)
              (vlax-put-property concells "item" x (+ 3 nsd) "0")
            )
          )
        )
      );progn
    );if
    (setq x (1+ x))
  );repeat
	 
  (setq x 1)
  (repeat (length lstlc)
    (setq n 1)
    (repeat (length (nth (- x 1) lstlc))
      (setq f (entget (nth (- n 1) (nth (- x 1) lstlc))))
      (setq e 0)
      (setq tens (list "List of 10s for the given force"))
      (repeat (length f)
        (if (= (nth 0 (nth e f)) 10)
          (setq tens (append tens (list (nth e f))))
        );if
        (setq e (1+ e))
      );repeat
      (setq tens (cdr tens))
      (setq lstf (append (list(cdr (assoc 304 f))) (list(cdr (nth 1 tens))) (list(cdr (nth 2 tens)))))
      
      (setq e 0)
      (setq leadervec '("Leader Vector"))
      (setq leaderlength 0)
      (repeat nsd
        (setq leadervec (append leadervec (list(- (nth e (nth 2 lstf)) (nth e (nth 1 lstf))))))
        (setq leaderlength (+ leaderlength (expt (- (nth e (nth 2 lstf)) (nth e (nth 1 lstf))) 2.0)))
        (setq e (+ 1 e))
      );repeat
      (setq leadervec (cdr leadervec))
      (setq leaderlength (sqrt leaderlength))
      
      (setq e 0)
      (repeat nsd
        (vlax-put-property loadcells "item" n (- (* x nsd 2) (- (* nsd 2) (+ e 1))) (rtos (nth e (nth 2 lstf)) 2 4))
        (vlax-put-property loadcells "item" n (- (* x nsd 2) (- (* nsd 2) (+ e 1 nsd))) (rtos (* (atof (nth 0 lstf)) (/ (nth e leadervec) leaderlength)) 2 4))
        (setq e (+ 1 e))
      );repeat
      
      (setq n (1+ n))
    );repeat
    (setq x (1+ x))
  );repeat

  (setq x 0)
  (setq ndv 0)
  (setq ndp 0)
  (setq ndg 0)
  (if ss3dpl
    (progn
      (repeat (sslength ss3dpl)
	(setq 3dpl (ssname ss3dpl x))
	(setq 3dptlst (reverse(cdr(reverse(3dptget 3dpl)))))
	(cond
	  ((= 2 (length 3dptlst)) ;case 1, vector
	    (progn
	      (setq n 1)
	      (repeat nsd
		(vlax-put-property dvcells "item" (+ ndv 1) n (rtos (nth (- n 1) (nth 0 3dptlst)) 2 4))
		(vlax-put-property dvcells "item" (+ ndv 1) (+ n nsd) (rtos (nth (- n 1) (nth 1 3dptlst)) 2 4))
		(setq n (+ 1 n))
              );repeat
	      (setq ndv (+ ndv 1))
	    );progn
	  );case 1, vector
	  
	  ((= 3 (length 3dptlst)) ;case 2, plane
	    (progn
	      (if (= nsd 3)
		(progn
		  (setq pnlst (planenormal 3dptlst))
	  	  (setq n 1)
		  (repeat nsd
		    (vlax-put-property dpcells "item" (+ ndp 1) n (rtos (nth (- n 1) (nth 0 pnlst)) 2 4))
		    (vlax-put-property dpcells "item" (+ ndp 1) (+ n nsd) (rtos (nth (- n 1) (nth 1 pnlst)) 2 4))
		    (setq n (+ 1 n))
	          );repeat
		  (setq ndp (+ ndp 1))
		);progn
              );if
	    );progn
	  );case 2, plane
	  
	  ((< 3 (length 3dptlst)) ;case 3, design group
	    (progn
	      (setq n 1)
	      (repeat (length 3dptlst)
		(setq e 1)
		(repeat nsd
		  (vlax-put-property dgcells "item" n (+ (* ndg nsd) e) (rtos (nth (- e 1) (nth (- n 1) 3dptlst)) 2 4))
		  (setq e (+ 1 e))
		);repeat nsd
		(setq n (+ n 1))
              );repeat 3dptlst
	      (setq ndg (+ ndg 1))
	    );progn
	  );case 3, design group
	);cond
	(setq x (+ x 1))
      );repeat ss3dpl
    );progn
  );if
  (vlax-put-property excel-app 'calculation -4105)
  (vlax-invoke-method wb-collection 'Close :vlax-true)
  (vlax-release-object wb-collection)
  (gc)
  (varres)
  (princ)
)
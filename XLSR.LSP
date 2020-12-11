(defun c:xlsr ()
  (varget)
  (setq excel-app (vlax-get-or-create-object "excel.application"))
  (setq filepath (getfiled "Geometry File" (getvar "dwgprefix") "xlsm" 2))
  (setq wb-collection (vl-catch-all-apply 'vla-open (list (vlax-get-property excel-app "workbooks") filepath)))
  (setq sheets (vl-catch-all-apply 'vlax-get-property (list wb-collection 'sheets)))
  ;(setq sheet1 (vlax-get-property sheets "item" 1))
  
  (initget "T D")
  (setq resulttype (getstring "\nEnter type of Results to Print: [Topology/Deformation]: "))
  
  (setq txthgt 0.025)

  (setq n 1)
  (repeat (vlax-get-property sheets "count")
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Parameters")
      (setq Paramcells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Topology")
      (setq topocells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Deformation")
      (setq defcells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Moment")
      (setq momcells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Axial")
      (setq axecells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (if (= (vlax-get (vlax-get-property sheets "item" n) "name") "Gradient")
      (setq gradcells (vlax-get (vlax-get-property sheets "item" n) "cells"))
    );if
    (setq n (+ 1 n))
  );repeat
  
  (if (equal (strcase (substr resulttype 1 1)) "T")
    (progn
      (initget "A L")
      (setq iterprint (getstring "\nEither Print All iterations or Last iteration: [All/Last]: "))
      (setq resultcells topocells)
    );progn T
    (setq resultcells defcells)
  );if Topology
  
  (setq backpos 1)
  (setq searchstart 1)
  (while backpos
    (setq backpos (vl-string-position 92 filepath searchstart))
    (if backpos
      (setq searchstart (+ backpos 1))
    )
  );while
  (setq inputname (substr filepath (+ searchstart 1) (- (strlen filepath) 5 searchstart)))
  
  (setq nsd (atoi(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property paramcells "item" 3 2)) 'Value) 8))))
  (setq ndf (atoi(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property paramcells "item" 4 2)) 'Value) 8))))
  (setq niter (atoi(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property paramcells "item" 5 2)) 'Value) 8))))

  (setq e 1)
  (while (not (= (vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" 1 (* e nsd 2))) 'Value) 8)) ""))
    (setq e (+ e 1))
  );while, number of print cycles+1
  
  (if (equal (strcase (substr resulttype 1 1)) "T")
    (progn
      (if (equal (strcase (substr iterprint 1 1)) "A")
        (progn
          (setq cyclestart  1
                cycleend    (+ niter 1))
        );progn topo all
        (progn
          (setq cyclestart  (- e 1)
                cycleend    e)
        );progn topo last
      );if topo all or last
    );progn topology print
    (progn
      (setq cyclestart  1
            cycleend    e)
    );progn deformation print
  );if topology or deformation
  
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "nomutt" 1)
  
  (setq oldpts '("list of previous x1, y1, z1"))
  
  (setq c cyclestart)
  (while (< c cycleend) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main Cycle
    (setq ssres (ssadd))
    
    (if (equal (strcase (substr resulttype 1 1)) "T")
      (progn
        (setq volume (rtos (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property gradcells "item" c 2)) 'Value) 8))) 2 4))
        (setq prefix "Iteration ")
      );progn
      (progn
        (setq volume (rtos (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property gradcells "item" (+ niter 1) 2)) 'Value) 8))) 2 4))
        (setq prefix "Combo ")
      );progn
    );if
    
    (setq e 1)

    ;;;;;;;;;;;;;;;;;;;;;; Member by Member Cycle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (while (not (= (vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e 1)) 'Value) 8)) ""))
      
      (setq x1 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 1 (* (- c 1) nsd 2)))) 'Value) 8))))
      (setq y1 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 2 (* (- c 1) nsd 2)))) 'Value) 8))))

      (if (= nsd 3)
        (progn
          (setq z1 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 3 (* (- c 1) nsd 2)))) 'Value) 8))))
          (setq z2 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 6 (* (- c 1) nsd 2)))) 'Value) 8))))
          (setq x2 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 4 (* (- c 1) nsd 2)))) 'Value) 8))))
          (setq y2 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 5 (* (- c 1) nsd 2)))) 'Value) 8))))
        );progn
        (progn
          (setq z1 (atof volume)
                z2 (atof volume));setq
          (setq x2 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 3 (* (- c 1) nsd 2)))) 'Value) 8))))
          (setq y2 (atof(vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property resultcells "item" e (+ 4 (* (- c 1) nsd 2)))) 'Value) 8))))
        );progn
      );if
      
      (if (= e 1)
        (setq ix1 x1
              iy1 y1
              iz1 z1
        );setq
      );if
      
      (if (= c cyclestart)
        (setq oldpts (append oldpts (list (list x1 y1 z1))))
        (progn
          (apply 'command-s (append '("3dpoly") (list (nth 0 oldpts) (list x1 y1 z1)) '("")))
          (setq oldpts (append oldpts (list(list x1 y1 z1))))
          (setq oldpts (cdr oldpts))
        )
      )

      (apply 'command-s (append '("line") (list (list x1 y1 z1)(list x2 y2 z2)) '("")))
      (setq ssres (ssadd (entlast) ssres))
      (if (equal (strcase (substr resulttype 1 1)) "D")
        (progn
          (setq textangle (atan (/ (- (+ y2 0.00001) y1) (- (+ x2 0.00001) x1))))
          (setq antiangle (atan (/ -1 (/ (- (+ y2 0.00001) y1) (- (+ x2 0.00001) x1)))))
          
          (setq xoffset (* 1.1 txthgt (cos antiangle)))
          (setq yoffset (* 1.1 txthgt (sin antiangle)))

          (setq axial (vlax-variant-value (vlax-variant-change-type (vlax-get-property (vlax-variant-value (vlax-get-property axecells "item" e c)) 'Value) 8)))
          (setq txtinsert (list (+ (/(+ x2 x1) 2) xoffset) (+ (/(+ y2 y1) 2) yoffset) (/(+ z2 z1) 2)))
          (apply 'command-s (append '("text") (list txtinsert txthgt (/ (* 180 textangle) pi) (strcat (rtos (atof axial) 2 2) (if (< 0 (atof axial)) " Ten" " Com"))) '("")))
          ;(command "text" (list (+ (/(+ x2 x1) 2) xoffset) (+ (/(+ y2 y1) 2) yoffset) (/(+ z2 z1) 2)) txthgt (/ (* 180 textangle) pi) (strcat (rtos (atof axial) 2 2) (if (< 0 (atof axial)) " Ten" " Com")))(command)
          (setq ssres (ssadd (entlast) ssres))
        );progn
      );if

      (setq e (+ e 1))
    );while Member Cycle
    
    (if (= c cyclestart)
      (setq oldpts (cdr oldpts))
    )
    
    (setq blkname (strcat inputname " - " prefix (itoa (- c 1)) "   Volume " volume))
    
    (setq flag 0)
    (if (not (= nil (tblsearch "BLOCK" blkname)))
      (progn
        (setq flag 1)
      );progn
    );if
    
    (if (= flag 0)
      (command "block" blkname "o" "c" (list ix1 iy1 iz1) ssres "")
      ;(command "block" blkname (list ix1 iy1 iz1) "o" "c"  ssres "")
      (progn (command "insert" blkname (list ix1 iy1 iz1) "" "" "")(command))
    )
    
    (command-s "erase" ssres "")
    
    (repeat (sslength ssres)
      (setq ssres (ssdel (ssname ssres 0) ssres))
    );repeat
    (setq ssres nil)
    
    (setq c (+ c 1))
  );while print cycle
  
  (vlax-invoke-method wb-collection 'Close :vlax-true)
  (vlax-release-object wb-collection)
  (gc)
  (varres)
  (alert (strcat (itoa c) " result(s) imported from: " inputname))
  (princ)
)
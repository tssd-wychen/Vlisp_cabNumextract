(defun c:PGB_pagebase()
  (vl-load-com)
  (setvar "osmode" 0)
  (defun FIND_MULTI_PTY (en_pty key / );????????? ? ??,????????????,???en_pty
    (setq pty_dem (vl-remove-if-not '(lambda (x) (= (car x) key)) en_pty))
  )
  (defun SS2NDFTLR(ss filter)
    (vl-cmdf "select" ss "")
    (ssget "P" filter)
  )
  (defun SS2ENLST(ss)
    (setq enlst nil)
    (if ss
      (repeat (sslength ss)
	(setq enlst (cons (ssname ss (length enlst)) enlst))
      )
      (progn
	(alert "no entity in this selected set")
      )
    )
    enlst
  )
  (defun ESTI_LIN_PTS(linptpr ptlist / ps pf p_x norm ptlist_n pt2dlist_n ps2d distlst mindist i_mind)
    ;line terminal point (2 points);point set
    ;list: minimal distance + the point in set
    (setq norm (mapcar '- (nth 1 linptpr) (nth 0 linptpr)))
    (setq ps (trans (nth 0 linptpr) 1 norm));point start line
    (setq pf (trans (nth 1 linptpr) 1 norm));point finish line
    (setq p_x -1)
    (setq ptlist_n (mapcar '(lambda(ptx)(setq p_x (1+ p_x))(cons p_x (trans ptx 1 norm))) ptlist))
    (setq ptlist_n (vl-remove-if-not
		     '(lambda(ptx)
			(and (> (caddr pf) (cadddr ptx))
			     (< (caddr ps) (cadddr ptx))
			     )
			)
		     ptlist_n)
	  )
    (if ptlist_n
      (progn
	(setq pt2dlist_n (mapcar '(lambda(ptx)(list (cadr ptx)(caddr ptx))) ptlist_n))
	(setq ps2d (list (car ps)(cadr ps)))
	(setq distlst (mapcar '(lambda(ptx)(distance ps2d ptx)) pt2dlist_n))
	(setq mindist (apply 'min distlst))
	(setq i_mind (1- (length (member mindist (reverse distlst)))))
	(setq i_mind (car (nth i_mind ptlist_n)))
	(setq ptout (list mindist (nth i_mind ptlist)))
      )
      (setq ptout nil)
    )
  )
  (defun SELENASBASE();select entity as base the frame
    (setq basefilter '((8 . "PRINT,WAKU,Page No")); drawing base
	  pgffilter '((0 . "LWPOLYLINE")(8 . "PRINT")); drawing frame
	  pgnfilter '((0 . "TEXT")(8 . "PRINT")); drawing absolute page no
	  pgpfilter '((0 . "LINE")(8 . "WAKU")); drawing base point
	  pgn2filter '((0 . "TEXT")(8 . "Page No"));drawing real page no
    )
    (prompt "select the print base frame")
    (setq basess (ssget basefilter))
    (mapcar '(lambda(x y)(set x (SS2NDFTLR basess y)))
	    (list 'pgfss 'pgnss 'pgpss) (list pgffilter pgnfilter pgpfilter))
    (mapcar '(lambda(x y)(set x (SS2ENLST y)))
	    (list 'pgfenlst 'pgnenlst 'pgpenlst) (list pgfss pgnss pgpss))
    (setq pgbplst nil);from entities of line in layer WAKU to get the base points of each page
    (foreach pgpen pgpenlst
      (setq pgppty (entget pgpen))
      (setq ptlst (mapcar '(lambda(x)(trans (cdr (assoc x pgppty)) (cdr (assoc -1 pgppty)) 1)) '(10 11)))
      (setq seta (apply 'angle ptlst))
      (if (> seta pi)
        (mapcar 'set (list 'seta 'bi) (list (* (- seta pi) (/ 180 pi)) 0))
	(mapcar 'set (list 'seta 'bi) (list (* seta (/ 180 pi)) 1))
      )
      (if (and (> seta 3) (< seta 87))
	(setq pgbplst (cons (nth bi ptlst) pgbplst))
      )
    )
    (list pgfenlst pgnenlst pgbplst)
  )
  (defun CONSPGNO(basenlst pgbenlst / pgbbdpt1 pgbbdpt2 pgbbdpt pgbpty pgbd pgbdsel upppgbd pgbbptlst fitpgbbpt fitpgben pgnotxt)
    (foreach pgbeni pgbenlst; page no txt center point
      (setq pgbobj (vlax-ename->vla-object pgbeni))
      (vla-getboundingbox pgbobj 'pgbbdpt1 'pgbbdpt2)
      (setq pgbbdpt1 (vlax-safearray->list pgbbdpt1))
      (setq pgbbdpt2 (vlax-safearray->list pgbbdpt2))
      (setq pgbbdpt (mapcar '(lambda(lpi)(/ lpi 2)) (mapcar '+ pgbbdpt1 pgbbdpt2))
	    pgbbdpt (trans pgbbdpt pgbeni 1));center point
      (setq pgbenlst (subst (cons pgbbdpt pgbeni) pgbeni pgbenlst));
    );append cneter point of txt to page no in page no list
    (foreach basen basenlst;at here , page frame entity list
      (setq pgbpty (entget basen))
      (setq pgbd (mapcar 'cdr (FIND_MULTI_PTY pgbpty 10)));page bound
      (setq pgbdsel (mapcar '(lambda(ptx)(trans ptx basen 1)) pgbd))

(setq pgbdsel (vl-sort pgbdsel '(lambda(p1 p2)(> (cadr p1)(cadr p2)))))
      (setq upppgbd (list (car pgbdsel)(cadr pgbdsel)))
      (setq pgbbptlst (mapcar 'car pgbenlst))
      (setq fitpgbbpt (ESTI_LIN_PTS upppgbd pgbbptlst))
      (setq fitpgbbpt (cadr fitpgbbpt))
      (setq fitpgben (cdr (assoc fitpgbbpt pgbenlst)))
      (if fitpgben
	(progn
          (setq pgnotxt (cdr (assoc 1 (entget fitpgben))))
	  (setq pgnotxt (cddr (vl-remove 32 (vl-string->list pgnotxt))))
	  (setq pgnotxt (atoi (vl-list->string pgnotxt)))
        )
	(setq pgno -1)
      )
      (setq basenlst (subst (cons pgnotxt basen) basen basenlst))
    )
    basenlst
  )
  (defun CONSBPT(basenlst pgplst)
    (setq pgfenlst (mapcar 'cdr basenlst));??????????
    (foreach pgfen pgfenlst
      (setq pgfpty (entget pgfen))
      (setq pgbd (mapcar 'cdr (FIND_MULTI_PTY pgfpty 10)));page bound
      (setq pgbdsel (mapcar '(lambda(ptx)(trans ptx pgfen 1)) pgbd))
      (setq pgbdsel (vl-sort pgbdsel '(lambda(p1 p2)(< (car p1)(car p2)))))
      (setq lftpgbd (list (car pgbdsel)(cadr pgbdsel)))
      (setq fitpgbpt (ESTI_LIN_PTS lftpgbd pgplst))
      (setq fitpgbpt (cadr fitpgbpt))
      (setq newspgfen (append (list pgfen) (list fitpgbpt)))
      (setq pgfenlst (subst newspgfen pgfen pgfenlst))
    )
    (setq basenlst (mapcar '(lambda(x y)(cons (car x) y)) basenlst pgfenlst))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq baseenlist (SELENASBASE))
  (mapcar 'set (list 'pgfrenlist 'pgnoenlist 'pgbptlist) baseenlist)
  (setq pgfrenlist (CONSPGNO pgfrenlist pgnoenlist))
  (setq baseenlist (CONSBPT pgfrenlist pgbptlist))
  (setq enum_mode '((0 . ">")(1 . "<")))
  (while
    (not
      (wcmatch
	(rtos
	  (setq modei (getint "determine left shift{to delete a page}{1} or right shift{to spare a page}{0}"))
	  2 0)
	"0,1")
    )
  )
  (setq baseenlist (vl-sort baseenlist '(lambda(x1 x2)(eval (list (read (cdr (assoc modei enum_mode))) (car x1) (car x2))))))
  ;right shift -> use > and left shift -> use <
 
  (setq pgctfilter '((-4 . "<not") (8 . "WAKU,Page No,PRINT") (-4 . "not>")))
  (foreach pagebase_i baseenlist
    (setq pagebase_i+1 (cadr (member pagebase_i baseenlist)))
    (if pagebase_i+1
      (progn
        (setq pgben_i+1 (cadr pagebase_i+1))
        (setq pgbpty_i+1 (entget pgben_i+1))
        (setq pgbd (mapcar 'cdr (FIND_MULTI_PTY pgbpty_i+1 10)))
        (setq pgbdsel (mapcar '(lambda(ptx)(trans ptx pgben_i+1 1)) pgbd))
        (setq pgctss_i+1 (ssget "WP" pgbdsel pgctfilter))
        (setq pst (caddr pagebase_i+1))
        (setq pfn (caddr pagebase_i))
        (command "MOVE" pgctss_i+1 "" pst pfn "")
      )
    )
  )
)

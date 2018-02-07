(defun EXT_CbTp_INFO(tss tmklist / tmkbptlst tmken tmkobj tmkbdpt1 tmkbdpt2 tmkbpt
		     pclen pclentry pclpty pclpts pclptf pclptz pclptlst
		     until_pcl pclptlst_tmp index_pcl besttmkl pclptpr pbesttmkl index_tml
		     index_pcnen pcnen pt1 pt2 pt12hort pt12cert pt3 pt4
		     pt_list pct_filter pctss pcten)
;
  ;函数的临时量不能和主函数中正在使用的变量相同，输入输出量反倒无所谓
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun find_multi_pty (en_pty key / );输入为联合属性列表 和 键值,返回值为带相应键值的列表,测试量en_pty
    (setq pty_dem (vl-remove-if-not '(lambda (x) (= (car x) key)) en_pty))
  )
  (defun esti_lin_pts(linptpr ptlist / ps pf p_x norm ptlist_n pt2dlist_n ps2d distlst mindist i_mind)
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq tmkbptlst nil)
  (foreach tmklisti tmklist
    (setq tmken (cdr tmklisti))
    (setq tmkobj (vlax-ename->vla-object tmken))
    (vla-getboundingbox tmkobj 'tmkbdpt1 'tmkbdpt2)
    (setq tmkbdpt1 (vlax-safearray->list tmkbdpt1))
    (setq tmkbdpt2 (vlax-safearray->list tmkbdpt2))
    (setq tmkbpt (mapcar '(lambda(lpi)(/ lpi 2)) (mapcar '+ tmkbdpt1 tmkbdpt2)));lp point list
    (setq tmkbptlst (cons tmkbpt tmkbptlst));tmkbptlst: text mark base point list
  )
  (setq tmkbptlst (mapcar '(lambda(ptx)(trans ptx 0 1)) tmkbptlst))
  
  (setq index_tml -1);index text mark list
  (while (< index_tml 0)
    (setq pclen nil)
    (while (not pclen);power cable line entity
        (setq pclen (car (entsel "select the power cable Line in WD")))
        (setq pclentry (vlax-ename->vla-object pclen))
        (vla-Highlight pclentry :vlax-true)
        (PROMPT "<ENTER>:")
        (VL-CMDF pause)
        (vla-Highlight pclentry :vlax-false)
    )
    
    (setq pclpty (entget pclen))
    (setq pclpts (find_multi_pty pclpty 10));power cable line point start
    (setq pclpts (mapcar 'cdr pclpts))
    (cond
      ((= (length pclpts) 1);情况：line
       (progn
	 (setq pclptf (cdr (assoc 11 pclpty)))
	 (setq pclptlst (cons pclptf pclpts));ptlst是线的端点列表
       )
      )
      ((> (length pclpts) 1);情况：leader、lwpolyline
       (progn
	 (if (= (length (car pclpts)) 2)
	   (progn
	     (setq pclptz (cdddr (assoc 210 pclpty)))
	     (setq pclptlst (mapcar '(lambda (pl)(append pl pclptz)) pclpts));该类情况为二维点列表。增加到三维
	   )
	   (setq pclptlst pclpts)
	 )
       )
      )
    )
    (setq pclptlst (mapcar '(lambda (pl)(trans pl pclen 1)) pclptlst));将ptlst的点的参考系转到ucs
    
    (setq until_pcl (- (length pclptlst) 1))
    (setq pclptlst_tmp pclptlst)
    (setq index_pcl nil)
    (setq besttmkl nil);best text mark list
    (repeat until_pcl
      (setq pclptpr (list (car pclptlst_tmp) (cadr pclptlst_tmp)));pclptpr power cable line point pair
      (setq pbesttmkl (esti_lin_pts pclptpr tmkbptlst));
      (if (not besttmkl)
	(setq besttmkl pbesttmkl)
	(if pbesttmkl
	  (if (< (car pbesttmkl) (car besttmkl))
	    (setq besttmkl pbesttmkl)
	  )
	)
      )
      (if (= (car pbesttmkl) (car besttmkl))
	(setq index_pcl (cons 1 index_pcl))
	(setq index_pcl (cons 0 index_pcl))
      )
      (setq pclptlst_tmp (cdr pclptlst_tmp))
    )
    (setq index_tml (1- (length (member (cadr besttmkl) tmkbptlst))));index of best text mark 
  )
  (setq index_pcl (length (member 1 index_pcl)));power cable line index
  (setq index_pcnen (car (nth index_tml tmklist)));index of power cable no entity   tmklist is an arg
  (setq pcnen (ssname tss index_pcnen))
  
  (setq pt1 (nth (1- index_pcl) pclptlst)
        pt2 (nth index_pcl pclptlst)
	pt12hort (abs (- (car pt1) (car pt2)))
	pt12cert (abs (- (cadr pt1) (cadr pt2)))
  )
  (if (<  pt12cert pt12hort)
    (setq pt3 (subst (+ (* 2 (car besttmkl)) (cadr pt2)) (cadr pt2) pt2)
	  pt4 (subst (+ (* 2 (car besttmkl)) (cadr pt1)) (cadr pt1) pt1))
    (setq pt3 (subst (- (car pt2) (* 2 (car besttmkl))) (car pt2) pt2)
	  pt4 (subst (- (car pt1) (* 2 (car besttmkl))) (car pt1) pt1))
  )
  (setq pt_list (list pt1 pt2 pt3 pt4))
  (setq pct_filter '((0 . "text")(1 . "*@*-#*")))
  (setq pctss (ssget "WP" pt_list pct_filter))
  (if pctss
    (if (= (sslength pctss) 1)
      (setq pcten (ssname pctss 0))
      (setq pcten "N/D");can't determined
    )
    (setq pcten nil)
  )
  (if pcten
    (if (= pcten "N/D")
      (setq outlist (list (cdr (assoc 1 (entget pcnen))) pcten))
      (setq outlist (list (cdr (assoc 1 (entget pcnen))) (cdr (assoc 1 (entget pcten)))))
    )
    (setq outlist (list (cdr (assoc 1 (entget pcnen))) "nil"))
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
)

;(setq norm (mapcar '- (trans (nth 2 pty_dem) en 1) (trans (nth 1 pty_dem) en 1)))
;(setq ps (trans (nth 2 pty_dem) en norm))
;(trans pt1 en1 1)
;(setq pp (trans pt1 en1 norm))
;(setq cz (list (car ps) (cadr ps) (caddr pp)))
;(TRANS cz norm 1)
(defun c:writbk()
  ;(vl-load-com)
  (setvar "cmdecho" 0)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (while txtList
    ;(setq eqp_Def_list (SLST_CSV2LSP eqp_Def_list))
    (if	(setq equip_En (SEL_BK_ISEQP))
      (progn
	(setq flag_Ar (car txtList))
	(cond
	  ((= flag_Ar "0")
	   (setq info_list (cons (cadr txtList) (member " " txtList)))
	   (setq info_circno (reverse (cdr (member " " (cdr (reverse (cdddr txtList)))))))
	   (setq info_list (append info_list info_circno))
       	   (MAPCAR 'ENTMOD (CLR_EQP_DEF equip_En));执行图元属性列表修改
	   (MOD_EQP_DEF equip_En info_list flag_Ar)
	  )
	  ((= flag_Ar "1")
	   (MOD_EQP_DEF equip_En info_list flag_Ar)
	  )
	  (t (exit))
	)
	;(setq excel_Col_i (1+ excel_Col_i))
      )
      (if (= (getstring "\n didn't select equip blk,quit or not [Y/N]") "Y")
	(exit)
	;()
      );未选择equipment block，询问退出否
    )
    (setq writed_ad_no (rtos writed_ad_no))
    (setq txtList nil)
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if writed_ad_no
    (princ (strcat "\n" writed_ad_no " attdefs have changed\n"))
    (princ "go to wiring diagram to get necessary information")
  )
  (setq writed_ad_no nil)
  (setq txtList nil)
)


(defun SEL_BK_ISEQP();让用户选择block，通过判断block是否有增强属性symb_no判断是否为需要修改的equip block
  ;返回该block的insert图元属性
  (if (setq eqp_en (entsel "\n select the equipment represented by block"))
    (progn
      (setq eqp_en (car eqp_en))
      (setq eqp_pty (entget eqp_en))
      (if (= (setq enkind (cdr (assoc 0 eqp_pty))) "INSERT")
	(if (any_attdef_is "SYMB_NO" eqp_en)
	  (setq eqp_en eqp_en)
	  (setq eqp_en nil)
	)
	(setq eqp_en nil)
      )
    )
    (setq eqp_en nil)
  )
)

(defun any_attdef_is(adname bken / loop_ad bken_pty bkadname);查询insert block是否有增强属性adname
  ;输入insert block图元名bken，待查增强属性adname，返回0
  (setq loop_ad 1)
  (while loop_ad
    (if (setq bken (entnext bken))
      (progn;下一图元非空
	(setq bken_pty (entget bken))
	     (if (= (cdr (assoc 0 bken_pty)) "ATTRIB")
	         (progn;图元是attrib时
		   (setq bkadname (cdr (assoc 2 bken_pty)))
	           (if (= bkadname adname)
	             (progn;图元是指定attdef
                       (setq loop_ad nil)
		       (setq flag_any_ad 1) 
		     )
	           )
		 )
	         (progn;有下一个en，不是attrib
		   (setq loop_ad nil)
		   (setq flag_any_ad nil)
		 )
	     )
      )
      (progn;下一图元为空时
	(setq loop_ad nil)
        (setq flag_any_ad nil)
      )
    )
  )
)

(defun CLR_EQP_DEF(bken / adenlst ad_en_i);找到所有要清除的insert block的增强属性并修改图元属性列表
  (setq ad_en_i (entnext bken))
  (setq adenlst nil)
  (while (= (cdr (assoc 0 (entget ad_en_i))) "ATTRIB")
    (setq adenlst (cons ad_en_i adenlst))
    (setq ad_en_i (entnext ad_en_i))
  )
  (setq adenptylst (mapcar 'entget adenlst))
  (setq adenptylst
	 (mapcar
	   (function (lambda(pty)
		       (if (not (wcmatch (cdr (assoc 2 pty)) "TEXT*"))
		         (setq pty (subst (cons 1 "") (assoc 1 pty) pty))
		         ;(entmod pty)
		       )
		      )
	    )
		adenptylst))
)

(defun MOD_EQP_DEF (en dlist flag_Ar
		    /
		   en_pty)
  (setq en (entnext en))
  (setq writed_ad_no 0)
  (if (= flag_Ar "0");rewrite block
    (foreach adstr dlist
    		;防止中途报错，将en从输入量（blk图元）转换到attrib图元，将attrib图元转换到下一个
        (if
  	  (setq en_pty (still_attrib en))
	  (progn
	    (setq en_pty (subst (cons 1 adstr) (assoc 1 en_pty) en_pty))
	    (entmod en_pty)
	    (setq writed_ad_no (1+ writed_ad_no))
	    (setq en (entnext (cdr (assoc -1 en_pty))))
  	  )
        )
    )
  )
  (if (= flag_Ar "1");append block
    (foreach adstr dlist
    		;防止中途报错，将en从输入量（blk图元）转换到attrib图元，将attrib图元转换到下一个
        (if
	  (setq en_pty (find_nil_attrib en))
	  (progn
	    (setq en_pty (subst (cons 1 adstr) (assoc 1 en_pty) en_pty))
	    (entmod en_pty)
	    (setq writed_ad_no (1+ writed_ad_no))
	    (setq en (entnext (cdr (assoc -1 en_pty))))
  	  )
        )
    )
  )
)

(defun still_attrib (en
		     /
		    );从输入的en找到attdef blk的下一个需要的attrib
  (if en
    (progn
      (setq en_pty (entget en))
      (setq flag_still_attrib (= (cdr (assoc 0 en_pty)) "ATTRIB"))
      (if flag_still_attrib
	(while flag_still_attrib
	  (setq adtag (cdr (assoc 2 en_pty)))
	  (if (wcmatch adtag
		       "SYMB_NO*,AREA_GROUP,AREA_NAME,ORDER,CIRC_NO#*,DIAGRAM_P,MACH_TYPE"
	      )
	    (progn
	      (setq flag_still_attrib nil)
	      (setq en_pty en_pty)
	    )
	    (if	(setq en (entnext en))
	      (progn
		(setq en_pty (entget en))
		(setq flag_still_attrib
		       (= (cdr (assoc 0 en_pty)) "ATTRIB")
		)
		(if (not flag_still_attrib)
		  (setq en_pty nil)
		)
	      )
	      (progn
		(setq flag_still_attrib nil)
		(setq en_pty nil)
	      )
	    )
	  )
	)
	(progn;若输入的en不是attrib
	  (setq en_pty nil)
	)
      )
    )
    (progn
      (setq en_pty nil)
    )
  )
)

(defun find_nil_attrib(en /
		       );寻找下一个内容为空的insert的增强属性attrib，返回图元属性
  (while
    (and
      (setq en_pty (still_attrib en))
      (if (/= (cdr (assoc 1 en_pty)) "")
	T
	(not (wcmatch (cdr (assoc 2 en_pty)) "CIRC_NO*"))
      )
    )
    (setq en (entnext (cdr (assoc -1 en_pty))))
  )
  (if en_pty
    (setq en_pty (entget en))
    nil
  )
)
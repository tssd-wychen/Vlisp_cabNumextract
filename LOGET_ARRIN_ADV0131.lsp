(defun c:writbk()
  ;(vl-load-com)
  (setvar "cmdecho" 0)
  (setq info_list (list (cadr txtList) (member " " txtList)))
  (setq )
  (setq flag_Ar (car info_list))
  ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (while (setq eqp_Def_list (read-line temp_File))
    (setq eqp_Def_list (SLST_CSV2LSP eqp_Def_list))
    (if	(setq equip_En (SEL_BK_ISEQP))
      (progn
	(setq flag_Ar (car eqp_Def_list))
	(setq eqp_Def_list (cdr eqp_Def_list))
	(cond
	  ((= flag_Ar "1")
       	   (MAPCAR 'ENTMOD (CLR_EQP_DEF equip_En));ִ��ͼԪ�����б��޸�
	   (MOD_EQP_DEF equip_En eqp_Def_list flag_Ar)
	  )
	  ((= flag_Ar "0")
	   (MOD_EQP_DEF equip_En eqp_Def_list flag_Ar)
	  )
	)
	;(setq excel_Col_i (1+ excel_Col_i))
      )
      (if (= (getstring "\n didn't select equip blk,quit or not [Y/N]") "Y")
	(exit)
	;()
      );δѡ��equipment block��ѯ���˳���
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (close temp_File)
  ;(VL-FILE-DELETE tempf_Path_s)
  ;();��ʾ�洢����info�������
  (setq writed_ad_no (rtos writed_ad_no))
  (princ (strcat "\n" writed_ad_no " attdefs have changed\n"))
)

(defun TEMPF_RINIT( / )
		  ;tempf_Path_s tempf_Path simf_exist)
  (setq tempf_Path (vl-filename-mktemp "linno.csv"))
  (princ (strcat "\n temp file path is" (setq tempf_Path (vl-filename-directory tempf_Path)) "\\linnoxxx.csv"))
  ;����linno003��linno004��������linno005������w��ʽ��linno004
  (if (setq simf_exist
	     (car
	       (reverse
		 (vl-directory-files tempf_Path "linno*.csv")
		 )
	       )
	    )
    
    (progn
      (setq tempf_Path_s (strcat tempf_Path "\\" simf_exist))
      (setq temp_File (open tempf_Path_s "r"))
    )
    (progn
      (alert "have not run 'COLLBK' command before,please run it to collect line no s 4 blk")
      (exit)
    )
  )
)

(defun SLST_CSV2LSP(strlist / str_i strlist)
  (setq llst nil)
  (setq strlist (strcat strlist ","))
  (while (setq i_c (vl-string-search "," strlist))
    (setq str_i (substr strlist 1 i_c))
    (setq llst (append llst (list str_i)))
    (setq strlist (substr strlist (+ i_c 2)))
  )
  (setq llst llst)
)

(defun SEL_BK_ISEQP();���û�ѡ��block��ͨ���ж�block�Ƿ�����ǿ����symb_no�ж��Ƿ�Ϊ��Ҫ�޸ĵ�equip block
  ;���ظ�block��insertͼԪ����
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

(defun any_attdef_is(adname bken / loop_ad bken_pty bkadname);��ѯinsert block�Ƿ�����ǿ����adname
  ;����insert blockͼԪ��bken��������ǿ����adname������0
  (setq loop_ad 1)
  (while loop_ad
    (if (setq bken (entnext bken))
      (progn;��һͼԪ�ǿ�
	(setq bken_pty (entget bken))
	     (if (= (cdr (assoc 0 bken_pty)) "ATTRIB")
	         (progn;ͼԪ��attribʱ
		   (setq bkadname (cdr (assoc 2 bken_pty)))
	           (if (= bkadname adname)
	             (progn;ͼԪ��ָ��attdef
                       (setq loop_ad nil)
		       (setq flag_any_ad 1) 
		     )
	           )
		 )
	         (progn;����һ��en������attrib
		   (setq loop_ad nil)
		   (setq flag_any_ad nil)
		 )
	     )
      )
      (progn;��һͼԪΪ��ʱ
	(setq loop_ad nil)
        (setq flag_any_ad nil)
      )
    )
  )
)

(defun CLR_EQP_DEF(bken / adenlst ad_en_i);�ҵ�����Ҫ�����insert block����ǿ���Բ��޸�ͼԪ�����б�
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
  (if (= flag_Ar "1");rewrite block
    (foreach adstr dlist
    		;��ֹ��;������en����������blkͼԪ��ת����attribͼԪ����attribͼԪת������һ��
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
  (if (= flag_Ar "0");append block
    (foreach adstr dlist
    		;��ֹ��;������en����������blkͼԪ��ת����attribͼԪ����attribͼԪת������һ��
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
		    );�������en�ҵ�attdef blk����һ����Ҫ��attrib
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
	(progn;�������en����attrib
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
		       );Ѱ����һ������Ϊ�յ�insert����ǿ����attrib������ͼԪ����
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
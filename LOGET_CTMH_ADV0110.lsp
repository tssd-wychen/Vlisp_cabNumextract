(defun c:CTG()
;full name of fcn: Cable no and Type Get
  (defun >STORE_SYSVAR()
    (setq insrt_Diapop (getvar "ATTDIA"))
    (setq insrt_Ptyget (getvar "ATTREQ"))
    (setq objsr_Modeon (getvar "osmode"))
    (setq Layer_Currnt (getvar "clayer"))
    (setq color_Nexten (getvar "cecolor"))
    (setq width_Next_L (getvar "celweight"))
    (setq typel_Next_L (getvar "celtype"))
  )
  (defun >RESTORE_SYSVAR()
    (setvar "ATTDIA" insrt_Diapop)
    (setvar "ATTREQ" insrt_Ptyget)
    (setvar "osmode" objsr_Modeon)
    (setvar "clayer" Layer_Currnt)
    (setvar "cecolor" color_Nexten)
    (setvar "celweight" width_Next_L)
    (setvar "celtype" typel_Next_L)
  )
  (defun >INIT_ORDER(/ PWD_feature LWD_feature CWD_feature AWD_feature order)
    ;(setq dwg_path (getvar "dwgprefix"))
    (setq dwg_name (vl-filename-base (getvar "dwgname")))
    (setq PWD_feature "*Power*,*power*,*42001*,*POWER*")
    (setq LWD_feature "*LT*,*42002*")
    (setq CWD_feature "*IC*,*I`.C`.*,*42003*")
    (setq AWD_feature "*Auto*,*auto*,*42004*,*AUTO*")
    (cond ((wcmatch dwg_name PWD_feature) (setq *orDer "P"))
	  ((wcmatch dwg_name LWD_feature) (setq *orDer "L"))
	  ((wcmatch dwg_name CWD_feature) (setq *orDer "C"))
	  ((wcmatch dwg_name AWD_feature) (setq *orDer "A"))
	  (t (setq *orDer ""))
    )
    (while (not
	     (wcmatch
	       (setq order (getstring (strcat "To determine the ORDER [default:" *orDer "]")))
	       "P,p,L,l,C,c,A,a,"
	     )
	   )
    )
    (if (and (= *orDer "") (= order ""))
      (progn
	(setq *orDer "L")
	(princ "ORDER is set as \"L\" ")
      )
      (cond
	((= order "p")(setq *orDer "P"))
	((= order "P")(setq *orDer "P"))
	((= order "l")(setq *orDer "L"))
	((= order "L")(setq *orDer "L"))
	((= order "c")(setq *orDer "C"))
	((= order "C")(setq *orDer "C"))
	((= order "a")(setq *orDer "A"))
	((= order "A")(setq *orDer "A"))
      )
    )
  )
  (defun >GET_CTMHF_PATH(/ ctmh_path file_list ctmhf_flag)
    (setq ctmh_path (vl-filename-directory (vl-filename-directory (getvar "dwgprefix"))))
    (setq *DCN_PATH (strcat ctmh_path "\\DATA\\"));D Cable No PATH
    (setq file_list (vl-directory-files *DCN_PATH))
    (setq ctmhf_flag (vl-member-if '(lambda (str) (wcmatch str "CableNo#.csv")) file_list))
    (if (not ctmhf_flag)
      (progn
	(setq *DCN_PATH (getfiled "open the \"CableNoX.csv\"" "" "csv" 8))
	(if (/= *DCN_PATH nil)
	  (setq *DCN_PATH (vl-filename-directory *DCN_PATH))
	  (progn
	    (alert "you reject to select the customARCH standard file path")
	    (exit)
	  )
	)
      )
    )
    (princ (strcat "the customARCH's file path is positioned at " *DCN_PATH))
    (vl-propagate '*DCN_PATH);���ñ���propagate����ǰ���������д򿪺�δ����򿪵��ļ������ռ���
  )
  (defun SS2PLST(ss / i_s len_s plst)
    (setq plst nil)
    (setq i_s 0)
    (setq len_s (sslength ss))
    (repeat len_s
      (setq plst (cons (ssname ss i_s) plst))
      (setq i_s (1+ i_s))
    )
    (setq plst (mapcar 'entget plst))
    (setq lstout (mapcar '(lambda(pty) (cdr (assoc 1 pty))) plst))
  )
  (defun >GET_FROM_DCL()
    (setq op_Flag (atoi (get_tile "dia100")))
    (setq fn_flag (atoi (get_tile "dia200")))
    (if (= op_Flag 0)
      (progn
        (setq cirMk (get_tile "dia001"))
        (setq area_Group (get_tile "dia003"))
        (setq area_Name (get_tile "dia004"))
      )
    )
    (if (= fn_flag 0)
        (setq pcnt_tlst (list (get_tile "dia005")))
    )
  )
  (defun >MOD_IN_DCL()
    (if (= op_Flag 1)
      (progn
        (mode_tile "dia001" 1)
        (mode_tile "dia003" 1)
        (mode_tile "dia004" 1)
	(mode_tile "dia005" 1)
	(mode_tile "dia200" 1)
      )
    )
    (if (= op_flag 0)
      (progn
        (mode_tile "dia001" 0)
        (mode_tile "dia003" 0)
        (mode_tile "dia004" 0)
	(mode_tile "dia005" 0)
	(mode_tile "dia200" 0)
      )
    )
    (if (or (= fn_flag 1) (= op_flag 1))
      (mode_tile "dia005" 1);close the input function
      (mode_tile "dia005" 0)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (vl-load-com)
  (>STORE_SYSVAR)
  (setvar "ATTDIA" 0)
  (setvar "ATTREQ" 0)
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  ;ͨ����Ϣ�����롣*��ʾ��һ�ε�����
  (if (= *area_Group nil)
    (setq *area_Group "ACC")
  )
  (if (= *area_Name nil)
    (setq *area_Name "UPP")
  )
  (if (= *cirMk nil);circuit mark
    (setq *cirMk "L")
  )
  (if (= *orDer nil);������VAR: orDer��!!!!!
    (>INIT_ORDER);��ĳһϵͳͼ�״�����ʱ��order�ĳ�ʼ��
  )
  (if (= *DCN_PATH nil);��·��ָ����CableNoX�Ĵ��λ��
    (>GET_CTMHF_PATH)
  )
  ;;;;;;
  (setq dcltmpf_name (vl-filename-mktemp "ARBFM.dcl"))
  (setq dcltmpf_path (vl-filename-directory dcltmpf_name))
  (if (setq file_exist (car (reverse (vl-directory-files dcltmpf_path "ARBFM*.dcl" 1))))
    (progn
      (setq dcltmpf_name (strcat dcltmpf_path "\\" file_exist))
      (setq dcltmpf_en (open dcltmpf_name "w"))
    )
    (setq dcltmpf_en (open dcltmpf_name "w"))
  )
  (foreach line_stream
    '("\n"
    "LOGET4ARR:dialog{\n"
    "  label=\"please input comm info of the equip\";\n"
    "  :row{\n"
    "    :toggle{\n"
    "        label=\"add circuit No at secondary side of the equipment\";\n"
    "        key=\"dia100\";\n"
    "        value=\"0\";\n"
    "      }\n"
    "    }\n"
    "  :row{\n"
    "    :toggle{\n"
    "        label=\"select power cable line or...\";\n"
    "        key=\"dia200\";\n"
    "        value=\"1\";\n"
    "      }\n"
    "    :edit_box{\n"
    "        key=\"dia005\";\n"
    "        height=1.2;\n"
    "        fixed_height=true;\n"
    "        edit_width=25;\n"
    "      }\n"
    "    }\n"
    "  :row{\n"
    "    :edit_box{\n"
    "        label=\"AREA_GROUP\";\n"
    "        key=\"dia003\";\n"
    "        height=1.2;\n"
    "        fixed_height=true;\n"
    "        edit_width=25;\n"
    "      }\n"
    "    }\n"
    "  :row{\n"
    "    :edit_box{\n"
    "        label=\"AREA_NAME\";\n"
    "        key=\"dia004\";\n"
    "        height=1.2;\n"
    "        fixed_height=true;\n"
    "        edit_width=25;\n"
    "      }\n"
    "    }\n"
    "  :row{\n"
    "    :edit_box{\n"
    "        label=\"CircuitMark_of_POWER_LINE\";\n"
    "        key=\"dia001\";\n"
    "        height=1.2;\n"
    "        fixed_height=true;\n"
    "        edit_width=25;\n"
    "      }\n"
    "    }\n"
    "  ok_cancel;\n"
    "}\n")
    (princ line_stream dcltmpf_en)
  )
  (close dcltmpf_en)
  
  (setq dcl_reply (load_dialog dcltmpf_name))
  (if (not (new_dialog "LOGET4ARR" dcl_reply))
    (exit)
  )
  (set_tile "dia003" *area_Group)
  (set_tile "dia004" *area_Name)
  (set_tile "dia001" *cirMk)
  (action_tile "dia100" "(>GET_FROM_DCL)(>MOD_IN_DCL)")
  (action_tile "dia200" "(>GET_FROM_DCL)(>MOD_IN_DCL)")
  (action_tile "accept" "(>GET_FROM_DCL)(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq dd (start_dialog))
  (unload_dialog dcl_reply)
  (cond ((= dd 1)(princ "common information for ARR equipment block has been loaded!"))
	((= dd 0)
	  (progn
	    (princ "you canceled the process")
	    (exit)
	  )
	)
  )

  (setq l_Type_filter '((1 . "@*-#*,(*),*`.*")));����Ĭ��fenceѡ��ʱ���ܳ��ֵķ��ߺŵ�text
  ;l_Type_filter�����˿����Ƿ��ߺŵ�text�ı�����������ͨ�����ʾ
  (prin1 "\nnow point or fence the line no\n")
  (setq txtSs (ssget '((0 . "TEXT"))));��ʾ����ȡuserѡ��ģ�fence��point��ѡ�� txtSs
  (setq pickOutss (ssget "P" l_Type_filter));��ǰ��ѡ����ѡ�����������ķ��ߺ�text����pickOutss
  (command "._Select" txtSs "_Remove" pickOutss "");��txtSs��ȥ��pickOutss
  (setq txtSs (ssget "P"))
  (setq loMklst (BMRK_LO txtSs));Ȧ��txtSs�е�ͼԪ
  (setq txtList (SS2PLST txtSs));ѡ�񼯻��ִ��б�
  (setq txtList (TLST_REFINE txtList cirMk))
  
  (if (= op_Flag 0)
    
    (progn
      (setq txtList (append txtList (list " " area_Group area_Name *orDer " ")));д��txtList ��׺space��G��N��O��space
      (if (= fn_flag 1)
        (setq pcnt_tlst (EXT_CbTp_INFO txtSs loMklst));����д���̣�EXTRACT_PCable_Type
	(progn
	  (setq pcnen (car (entsel "select the power cable no")))
	  (setq pcnt (cdr (assoc 1 (entget pcnen))))
	  (setq pcnt_tlst (cons pcnt pcnt_tlst))
	)
      )
      (setq pcnt_tlst (append (list (car (TLST_REFINE (list (car pcnt_tlst)) cirMk))) (cdr pcnt_tlst)))
      (setq txtList (vl-remove-if '(lambda(strx)(= strx (car pcnt_tlst))) txtList))
      (setq txtList (cons (car pcnt_tlst) txtList))
      (setq txtList (append pcnt_tlst txtList));д��txtList ǰ׺W/A,PC,PCT
      (setq txtList (cons (rtos op_Flag 2 0) txtList))
    )
    (setq txtList (cons (rtos op_Flag 2 0) txtList));д��txtList ǰ׺W/A
  )
  (vl-propagate 'txtList)
  
  ;;;;;;
  (if (= area_Group "")
    (setq area_Group *area_Group)
    (setq *area_Group area_Group)
  )

  (if (= area_Name "")
    (setq area_Name *area_Name)
    (setq *area_Name area_Name)
  )
  (if (= cirMk "")
    (setq cirMk *cirMk)
    (setq *cirMk cirMk)
  )
  (>RESTORE_SYSVAR)
  (prin1)

)

(defun BMRK_LO (ss / i sslen eni eni_pty) ;name: batch mark line no . arg: selected set with pure text entity . return: index of
  ;select set and mark rectang entity one to one list
  (setq layer-1 (getvar "clayer"))
  (setq mrklaname "CTG-4ARR-4CTMH-CNX")	;Cable no and Type Get for ARR & for CusTomMarcH Cable No X
  (if (not (tblsearch "layer" mrklaname)) ;������ͼͼ��
    (progn
      (command "layer" "N" mrklaname "C" "1" mrklaname "")
      (setvar "clayer" mrklaname)
    )
    (setvar "clayer" mrklaname)
  )
  (setvar "celweight" 30)
  (setq lmklst nil)			;line no mark list ��Ա���Ӧss�����0,1...+markͼԪ��
  (setq i 0)
  (setq sslen (sslength ss))
  (repeat sslen
    (setq eni (ssname ss i))
    (setq eni_pty (entget eni))
    (setq eni (MARK_LO eni_pty))
    (setq eni (cons i eni))
    (setq i (1+ i))
    (setq lmklst (append lmklst (list eni)))
  )
  (setvar "clayer" layer-1)
  (setq lmklst lmklst)
)
(defun MARK_LO(txt_en_pty /
	       txtbxpt txt_bp
	       txtbxp_list tbp_x
	       p1 p2 p3 p4);name: mark line no . arg: text entity's property(dxf code) . return: line no's mark entity (rectang entity)
;��ע�Ѿ�ѡ����ߺ��ı�
;��������text��dxf����
;������������ı�ע������ߣ���ͼԪ��
  (setq txtbxpt (textbox txt_en_pty)
	txt_bp (trans (cdr (assoc 10 txt_en_pty)) (cdr (assoc -1 txt_en_pty)) 1)
        txtbxp_list nil)
  (setq txtangle (cdr (assoc 50 txt_en_pty)))
  (foreach tbp_x txtbxpt
    (setq tbp_x (list (- (* (cos txtangle) (car tbp_x)) (* (sin txtangle) (cadr tbp_x)))
		      (+ (* (sin txtangle) (car tbp_x)) (* (cos txtangle) (cadr tbp_x)))
		)
    )
    (setq tbp_x (list (+ (car txt_bp) (car tbp_x)) (+ (cadr txt_bp) (cadr tbp_x))))
    (setq txtbxp_list (cons tbp_x txtbxp_list))
  )
  (command "_rectang" (nth 0 txtbxp_list) "r" (* (/ txtangle pi) 180) (nth 1 txtbxp_list) "")
  (entlast)
)



(defun TLST_REFINE(lo_str_list cirMk / asclastlst seq tlst_tmp);name: text list refine . arg: pure string list . return: pure string list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rem_sufprefix(strx / ax)
      ;remove the suffix not necessary
      ;input: str. output: str.
      (setq ax (cdr (member 32 (reverse (vl-string->list strx)))))
      (if (member 32 ax);32->space
          (setq strx (vl-list->string (reverse ax)))
      )
      (if (setq ax (cdr (member 40 (reverse (vl-string->list strx)))));40->(....
          (setq strx (vl-list->string (reverse ax)))
      )
      (setq strx (vl-list->string (vl-remove 32 (vl-string->list strx))))
    )
  (defun merge_CM_if_not(cirMk lo_str_list / i_slt feature_CM)
    ;merge circuit mark and line no
    ;for some circuit
      (setq feature_CM "#L#*,#L@*,#E#*,#E@*");����circuit mark��line no�ִ�������
      (setq feature_CM (strcat feature_CM "," cirMk "#*" "," cirMk "@*"))
      (foreach strx lo_str_list
        (if (and (not (wcmatch strx feature_CM)) (wcmatch strx "#*"))
          ;�޷������п���û��circuit mark���ִ������жϲ���ӣ�
          ;�����Ƿ��ϸ����������������ֿ�ͷ���ߺ��ִ����circuit mark
          (setq lo_str_list (subst (strcat cirMk strx) strx lo_str_list))
        )
      )
      (setq lo_str_list lo_str_list)
    )
  (defun disperselo_in_lst (lo_str_list / add_lst
			    strlt_tmp orgstr punc
			    str2 alen root add_lst_i)
      (setq add_lst (list "start!"))
      (while add_lst
        (setq add_lst nil)
        (foreach lostri lo_str_list
          (if (vl-string-search "," lostri)
  	    (progn
              (setq strlt_tmp nil);�����ֶ��ŵ��ִ��ָ���б����strlt_tmp,temperary string list
              (setq orgstr lostri)
              (while (setq punc (vl-string-search "," lostri))
                (setq strlt_tmp (cons (substr lostri 1 punc) strlt_tmp))
	        (setq lostri (substr lostri (+ punc 2)))
              )
              (setq strlt_tmp (reverse (cons lostri strlt_tmp)))
	      (setq strlt_tmp (mapcar 'rem_sufprefix strlt_tmp))
              (setq str2 (cadr strlt_tmp))
              (if (and (not (wcmatch str2 "*~*")) (not (wcmatch str2 "* *")));��ȡ�ִ�������δʡ�Բ��ֳ���
                (setq alen (strlen str2))
	        (setq alen (min (vl-string-search "~" str2) (vl-string-search " " str2)))
              )
              ;��ȡ�ִ�������ʡ�Բ�������
              (setq root (substr (car strlt_tmp) 1 (- (strlen (car strlt_tmp)) alen)))
              (setq lo_str_list (subst (car strlt_tmp) orgstr lo_str_list));�滻ԭ�ִ�Ϊ��һ���ߺ�
              (setq add_lst_i (mapcar '(lambda (str)(strcat root str)) (cdr strlt_tmp)))
              (setq add_lst (append add_lst add_lst_i))
	    )
          )
        )
        (if (/= add_lst nil)
          (setq lo_str_list (append lo_str_list add_lst))
        )
      )
      (setq lo_str_list lo_str_list)
    )
  ;(defun CNseq (cntxt / asclst)
   ;   (setq asclst (vl-string->list cntxt))
    ;  (setq asclast (car (reverse asclst)))
    ;)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq lo_str_list (mapcar 'rem_sufprefix lo_str_list))
    (setq lo_str_list (merge_CM_if_not cirMk lo_str_list))
    (setq lo_str_list (disperselo_in_lst lo_str_list))
    ;(setq asclastlst (mapcar 'CNseq lo_str_list))
    ;(setq seq (vl-sort-i asclastlst '<))
    ;(setq tlst_tmp nil)
    ;(foreach ix seq
    ;  (setq tlst_tmp (append tlst_tmp (list (nth ix lo_str_list))))
    ;)
    ;(setq tlst_out tlst_tmp)

  )

;;;  ====================================
;;;  PEACE zhptj1986@gmail.com
;;;  http://blog.sina.com.cn/peacelvirene
;;;  ZIAD�㽭ʡ��������о�Ժ
;;;  ====================================
;;;  �����������༭
;;;  ���PEACE-BProp
;;;  by PEACE 2013/08/20 V1.0
;;;  ====================================
(vl-load-com)
(defun c:PEACE-BProp ( / 
               ;�ֲ�����
               *error*
               PEACE:StoreSysVarCAD
               PEACE:RestoreSysVarCAD
               PEACE:SaveSysVarPeace
               PEACE:ReadSysVarPeace
               PEACE:Fsxm-ssget
               SaveSysVar
               GETDATA
               ;�ֲ�����
               vcmde vblip vclay vosmo vplwd vlupr vdelo vtsty ;ϵͳ����
               dclname tempname filen stream dcl_re
               selobjs stylelist style stylen layerlist layer layern
               colorlist colorn ss_name i ss_att blocklist block blockn
                    )
;�ֲ�������ʼ
;�Զ����������
(defun *error* (msg)
  (PEACE:RestoreSysVarCAD) ;��ԭϵͳ����
  (command ".UNDO" "E")
  (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
      (princ (strcat "\n** Error: " msg " **")))
  (princ)
)
;����ϵͳ����
(defun PEACE:StoreSysVarCAD()
  (setq vcmde (getvar "cmdecho"))  ;��ͨ�������ʾ
  (setq vblip (getvar "blipmode")) ;���ۼ�
  (setq vclay (getvar "CLAYER"))   ;ͼ��
  (setq vosmo (getvar "osmode"))   ;��׽ģʽ
  (setq vplwd (getvar "plinewid")) ;pl���
  (setq vlupr (getvar "luprec"))   ;���Ⱦ���
  (setq vdelo (getvar "delobj"))   ;���ƴ�������ʱ�Ƿ���ԭpline��0Ϊ������1Ϊ������
  (setq vtsty (getvar "textstyle"))
)
;��ԭϵͳ����
(defun PEACE:RestoreSysVarCAD()
  (setvar "cmdecho" vcmde)
  (setvar "blipmode" vblip)
  (setvar "CLAYER" vclay)
  (setvar "osmode" vosmo)
  (setvar "plinewid" vplwd)
  (setvar "luprec" vlupr)
  (setvar "delobj" vdelo)
  (setvar "textstyle" vtsty)
)
;����peaceϵͳ����
(defun PEACE:SaveSysVarPeace(valname valvalue infotext / acadpath f datalist data valvalue_old i isthere)
  (setq acadpath(vlax-get-property (vlax-get-acad-object) 'Path))
  (if (= infotext "")(setq infotext "no infotext"))
  (if (null (findfile "PEACESYSVAL.TXT"))
    (progn ;���ļ�������
      (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "w"))
      (prin1 (list valname valvalue infotext) f)
      (close f)
    )
    (progn ;���ļ��Ѵ���
      (setq datalist '())
      (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "r"))
        (while (setq data (read-line f))
	      (setq datalist (cons data datalist))
        )
      (close f)
      (setq datalist (reverse datalist))
      (setq       i 0
            isthere 0)
      (repeat (length datalist)
        (if (= valname (car (read (nth i datalist))))
          (progn
          (setq datalist (subst (vl-prin1-to-string (list valname valvalue infotext)) (nth i datalist) datalist))
          (setq isthere 1)
          )
        )
        (setq i (1+ i))
      )
      (if (= 1 isthere)
        (progn
          (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "w"))
          (prin1 (read (nth 0 datalist)) f)
          (close f)
          (setq i 1)
          (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "a"))
          (repeat (- (length datalist) 1)
            (write-line "" f)
            (prin1 (read (nth i datalist)) f)
            (setq i (1+ i))
          )
          (close f)
        )
        (progn
          (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "a"))
          (write-line "" f)
          (prin1 (list valname valvalue infotext) f)
          (close f)
        )
      ) 
    )
  )
  (princ)
)
;��ȡpeaceϵͳ����
(defun PEACE:ReadSysVarPeace( / acadpath data datalist i f)
  (setq acadpath(vlax-get-property (vlax-get-acad-object) 'Path))
  (if (findfile "PEACESYSVAL.TXT")
    (progn
    (setq datalist '())
    (setq f (open (strcat acadpath "\\PEACESYSVAL.TXT") "r"))
      (while (setq data (read-line f))
	    (setq datalist (cons data datalist))
      )
      (reverse datalist)
    (close f)
    (setq i 0)
    (repeat (length datalist)
      (set (read (car (read (nth i datalist)))) ;ע���ַ��ͱ�֮���ת�����ַ����ǲ�����Ϊ��������
           (cadr (read (nth i datalist)))       ;car���ַ���Ҳ�ǲ������õ�
      )
      (setq i (1+ i))
    )
    )
  nil
  )
)

;;���ؼ��ֵ�ssget
;;Msg=��ʾ��Ϣ��Kwd=�ؼ��֣�Fil=����
;ʾ����(PEACE:Fsxm-ssget "\n��ѡ��һ��Բ��" "F" '((0 . "circle")))
(defun PEACE:Fsxm-ssget (Msg Kwd Fil / Kwd0 pt var *acad* *doc*)
  (setq *acad* (vlax-get-acad-object))
  (setq *doc* (vla-get-ActiveDocument *acad*))
  ;===�ڲ�������ʼ===
  ;;����������entsel
  (defun Fsxm-entsel (msg filter)
    (setq enp (entsel msg))
    (if (or (= (type enp) 'str)
           (and enp (ssget (cadr enp) filter))
        )
     enp
    )
  )
  ;;;�÷ָ��������ַ����ɱ�
  (defun Fsxm-Split (string strkey / po strlst xlen)
    (setq xlen (1+ (strlen strkey)))
    (while (setq po (vl-string-search strkey string))
      (setq strlst (cons (substr string 1 po) strlst))
      (setq string (substr string (+ po xlen)))
    )
    (reverse (cons string strlst))
  )
  ;;�㻯�ִ�
  (defun Fsxm-Pt2Str (pt)
    (strcat (rtos (car pt) 2 2)
            ","
            (rtos (cadr pt) 2 2)
            ","
            (rtos (caddr pt) 2 2)
            "\n"
    )
  )
  ;===�ڲ���������===
  (cond 
        ((cadr (ssgetfirst)))
        (T
         (setq Kwd0 "W")
         (initget (strcat Kwd0 " " kwd))
         (cond ((and (listp (setq var (Fsxm-entsel Msg Fil)))
                     (/= 52 (getvar "errno"))
                )
                (vla-sendcommand *doc* (Fsxm-Pt2Str (cadr (grread t))))
                (ssget Fil)
               )
               ((member var (Fsxm-Split Kwd0 " "))
                (vla-sendcommand *doc* (strcat var "\n"))
                (ssget Fil)
               )
               (t var)
         )
        )
  )
)
(defun SaveSysVar()
  (PEACE:SaveSysVarPeace "PEACE:BP_H" PEACE:BP_H "PEACE-BProp���ָ߶�")
  (PEACE:SaveSysVarPeace "PEACE:BP_W2H" PEACE:BP_W2H "PEACE-BProp���ֿ�߱�")
  (PEACE:SaveSysVarPeace "PEACE:BP_S" PEACE:BP_S "PEACE-BProp������ʽ")
  (PEACE:SaveSysVarPeace "PEACE:BP_L" PEACE:BP_L "PEACE-BPropͼ����")
  (PEACE:SaveSysVarPeace "PEACE:BP_C" PEACE:BP_C "PEACE-BProp������ɫ")
  (PEACE:SaveSysVarPeace "PEACE:BP_B" PEACE:BP_B "PEACE-BProp����")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFB" PEACE:BP_IFB "PEACE-BProp�Ƿ�ָ������")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFL" PEACE:BP_IFL "PEACE-BProp�Ƿ��ͼ��")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFC" PEACE:BP_IFC "PEACE-BProp�Ƿ����ɫ")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFS" PEACE:BP_IFS "PEACE-BProp�Ƿ����ʽ")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFH" PEACE:BP_IFH "PEACE-BProp�Ƿ��߶�")
  (PEACE:SaveSysVarPeace "PEACE:BP_IFW" PEACE:BP_IFW "PEACE-BProp�Ƿ����")
)
(defun GETDATA()
  (setq   
            PEACE:BP_H (atof (get_tile "ea401"))
          PEACE:BP_W2H (atof (get_tile "ea501"))
          PEACE:BP_IFB (atoi (get_tile "ea000"))
          PEACE:BP_IFL (atoi (get_tile "ea100"))
          PEACE:BP_IFC (atoi (get_tile "ea200"))
          PEACE:BP_IFS (atoi (get_tile "ea300"))
          PEACE:BP_IFH (atoi (get_tile "ea400"))
          PEACE:BP_IFW (atoi (get_tile "ea500"))
  )
)
;�ֲ���������
;��������ʼ
  (PEACE:StoreSysVarCAD)  ;����ϵͳ����
  (PEACE:ReadSysVarPeace) ;��ȡpeaceϵͳ����
  (setvar "cmdecho" 0)
  (command ".UNDO" "BE")
  (princ "PEACE-BProp �����������༭")
  (if (not PEACE:BP_H)(setq PEACE:BP_H 350));���ָ߶�
  (if (not PEACE:BP_W2H)(setq PEACE:BP_W2H 0.7));���ֿ�߱�
  (if (not PEACE:BP_IFB)(setq PEACE:BP_IFB 0));����
  (if (not PEACE:BP_IFL)(setq PEACE:BP_IFL 0));ͼ��
  (if (not PEACE:BP_IFC)(setq PEACE:BP_IFC 0));��ɫ
  (if (not PEACE:BP_IFS)(setq PEACE:BP_IFS 0));��ʽ
  (if (not PEACE:BP_IFH)(setq PEACE:BP_IFH 0));�߶�
  (if (not PEACE:BP_IFW)(setq PEACE:BP_IFW 0));���
  (setq blocklist '())
  (vlax-for block 
            (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) 
            (setq blocklist  (cons (vla-get-name block) blocklist))
  )
  (setq blocklist (acad_strlsort blocklist))
  (setq stylelist '())
  (vlax-for style 
            (vla-get-textstyles (vla-get-activedocument (vlax-get-acad-object))) 
            (setq stylelist  (cons (vla-get-name style) stylelist))
  )
  (setq stylelist (acad_strlsort stylelist))
  (setq layerlist '())
  (vlax-for layer 
            (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) 
            (setq layerlist  (cons (vla-get-name layer) layerlist))
  )
  (setq layerlist (acad_strlsort layerlist))
  (setq i 1 colorlist '())
  (repeat 255
    (setq colorlist (cons (rtos i 2 0) colorlist))
    (setq i (1+ i))
  )
  (setq colorlist (reverse colorlist)
        colorlist (cons "Bylayer" colorlist)
  )
  (if (not PEACE:BP_B)(setq PEACE:BP_B (nth 0 blocklist)));����
  (if (not PEACE:BP_S)(setq PEACE:BP_S (nth 0 stylelist)));������ʽ
  (if (not PEACE:BP_L)(setq PEACE:BP_L (nth 0 layerlist)));ͼ��
  (if (not PEACE:BP_C)(setq PEACE:BP_C (nth 0 colorlist)));��ɫ
  (if (not (setq blockn (vl-position PEACE:BP_B blocklist))) (setq blockn 0))
  (if (not (setq stylen (vl-position PEACE:BP_S stylelist))) (setq stylen 0))
  (if (not (setq layern (vl-position PEACE:BP_L layerlist))) (setq layern 0))
  (if (not (setq colorn (vl-position PEACE:BP_C colorlist))) (setq colorn 0))
  (setq dclname 
    (cond
  	  ((setq tempname (vl-filename-mktemp "PEACEDCL.dcl")
  	  	        filen (open tempname "w")
  	  )
  	  (foreach stream 
	   '("\n" "dcl01:dialog {\n"
		 "    label = \"�����������༭\" ;\n"
		 "    :row { "
		 "      :toggle{label=\"ָ������    \" ; key = \"ea000\" ;  } \n"
		 "         }\n"
		 "    :row { \n"
		 "      :popup_list { label = \" \" ; key = \"ea001\" ; edit_width = 25 ;   height = 1.2 ;  } \n"
		 "         } \n"
		 "    :row { "
		 "      :toggle{label=\"ͼ������    \" ; key = \"ea100\" ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :popup_list { label = \" \" ; key = \"ea101\" ; edit_width = 25 ;   height = 1.2 ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :toggle{label=\"������ɫ    \" ; key = \"ea200\" ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :popup_list { label = \" \" ; key = \"ea201\" ; edit_width = 25 ;   height = 1.2 ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :toggle{label=\"������ʽ    \" ; key = \"ea300\" ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :popup_list { label = \" \" ; key = \"ea301\" ; edit_width = 25 ;   height = 1.2 ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :toggle{label=\"���ָ߶�\" ; key = \"ea400\" ;  } \n"
		 "      :edit_box { label = \" \" ; key = \"ea401\" ; edit_width = 15 ;   height = 1.2 ;  } \n"
		 "         }\n"
		 "    :row { "
		 "      :toggle{label=\"��߱���\" ; key = \"ea500\" ;  } \n"
		 "      :edit_box { label = \" \" ; key = \"ea501\" ; edit_width = 15 ;   height = 1.2 ;  } \n"
		 "         }\n"
		 "    ok_cancel;"
		 "    }"
		)
		(princ stream filen)
	    )
	    (close filen)
	    tempname
	  )
    )
  )
  (setq dcl_re (load_dialog dclname))
  (if (not (new_dialog "dcl01" dcl_re))
    (exit)
  )
  (mode_tile "accept" 2)
  (start_list "ea001")
  (mapcar 'add_list blocklist)
  (end_list)
  (start_list "ea101")
  (mapcar 'add_list layerlist)
  (end_list)
  (start_list "ea201")
  (mapcar 'add_list colorlist)
  (end_list)
  (start_list "ea301")
  (mapcar 'add_list stylelist)
  (end_list)
  (set_tile "ea001" (rtos blockn 2 0))
  (set_tile "ea101" (rtos layern 2 0))
  (set_tile "ea201" (rtos colorn 2 0))
  (set_tile "ea301" (rtos stylen 2 0))
  (set_tile "ea401" (rtos PEACE:BP_H 2 2))
  (set_tile "ea501" (rtos PEACE:BP_W2H 2 2))
  (set_tile "ea000" (rtos PEACE:BP_IFB 2 0))
  (set_tile "ea100" (rtos PEACE:BP_IFL 2 0))
  (set_tile "ea200" (rtos PEACE:BP_IFC 2 0))
  (set_tile "ea300" (rtos PEACE:BP_IFS 2 0))
  (set_tile "ea400" (rtos PEACE:BP_IFH 2 0))
  (set_tile "ea500" (rtos PEACE:BP_IFW 2 0))
  (if (= PEACE:BP_IFB 0)(mode_tile "ea001" 1)(mode_tile "ea001" 0))
  (if (= PEACE:BP_IFL 0)(mode_tile "ea101" 1)(mode_tile "ea101" 0))
  (if (= PEACE:BP_IFC 0)(mode_tile "ea201" 1)(mode_tile "ea201" 0))
  (if (= PEACE:BP_IFS 0)(mode_tile "ea301" 1)(mode_tile "ea301" 0))
  (if (= PEACE:BP_IFH 0)(mode_tile "ea401" 1)(mode_tile "ea401" 0))
  (if (= PEACE:BP_IFW 0)(mode_tile "ea501" 1)(mode_tile "ea501" 0))
  (action_tile "ea000" "(GETDATA)(COND ((= PEACE:BP_IFB 0)(mode_tile \"ea001\" 1))((= PEACE:BP_IFB 1)(mode_tile \"ea001\" 0)))")
  (action_tile "ea100" "(GETDATA)(COND ((= PEACE:BP_IFL 0)(mode_tile \"ea101\" 1))((= PEACE:BP_IFL 1)(mode_tile \"ea101\" 0)))")
  (action_tile "ea200" "(GETDATA)(COND ((= PEACE:BP_IFC 0)(mode_tile \"ea201\" 1))((= PEACE:BP_IFC 1)(mode_tile \"ea201\" 0)))")
  (action_tile "ea300" "(GETDATA)(COND ((= PEACE:BP_IFS 0)(mode_tile \"ea301\" 1))((= PEACE:BP_IFS 1)(mode_tile \"ea301\" 0)))")
  (action_tile "ea400" "(GETDATA)(COND ((= PEACE:BP_IFH 0)(mode_tile \"ea401\" 1))((= PEACE:BP_IFH 1)(mode_tile \"ea401\" 0)))")
  (action_tile "ea500" "(GETDATA)(COND ((= PEACE:BP_IFW 0)(mode_tile \"ea501\" 1))((= PEACE:BP_IFW 1)(mode_tile \"ea501\" 0)))")
  (action_tile "ea001" "(setq blockn $value)(setq PEACE:BP_B (nth (atoi blockn) blocklist))")
  (action_tile "ea101" "(setq layern $value)(setq PEACE:BP_L (nth (atoi layern) layerlist))")
  (action_tile "ea201" "(setq colorn $value)(setq PEACE:BP_C (nth (atoi colorn) colorlist))")
  (action_tile "ea301" "(setq stylen $value)(setq PEACE:BP_S (nth (atoi stylen) stylelist))")
  (action_tile "accept" "(GETDATA)(SaveSysVar)(done_dialog)")
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog dcl_re)
  (vl-file-delete dclname)
  (if (= PEACE:BP_IFB 0)
    (setq selobjs (PEACE:Fsxm-ssget "\n>>> ѡ���<�˳�>:" " " (list'(0 . "insert"))))
    (setq selobjs (PEACE:Fsxm-ssget "\n>>> ѡ���<�˳�>:" " " (list'(0 . "insert")(cons 2 PEACE:BP_B))))
  )
  (cond
    (
      (= selobjs nil)
      (princ "\n*** �����˳�!")
    )
    (
      t
      (setq i 0)
      (repeat (sslength selobjs)
        (setq ss_name (ssname selobjs i) ss_att (entget ss_name))
        (if (= (cdr (assoc 66 ss_att)) 1)
          (progn                        ;��������������(�����ǿ���Ա༭���е�����ѡ������ԣ�������ʹ�ñ��������û�����
            (mapcar '(lambda (x)
                       (if (= PEACE:BP_IFL 1)(vla-put-layer x PEACE:BP_L));ͼ��
                       (if (= PEACE:BP_IFC 1) ;��ɫ
                         (if (equal PEACE:BP_C "Bylayer")
                           (vla-put-color x acbylayer)
                           (vla-put-color x (atoi PEACE:BP_C))
                         )
                       )
                       (if (= PEACE:BP_IFS 1)(vla-put-stylename x PEACE:BP_S));��ʽ
                       (if (= PEACE:BP_IFH 1)(vla-put-height x PEACE:BP_H));�߶�
                       (if (= PEACE:BP_IFW 1)(vla-put-scalefactor x PEACE:BP_W2H));���
                     )
                     (safearray-value
                       (variant-value
                         (vla-getattributes
                           (vlax-ename->vla-object ss_name)
                         )
                       )
                     )
            )
          )
        )
        (setq i (1+ i))
      )
      (princ "\n*** �����Ա༭���!")
    )
  )
  (princ)
)

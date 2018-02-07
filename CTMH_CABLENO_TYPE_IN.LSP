(defun c:CTG_CTMHF_IN()
  (defun SELCTMHFILE(txt_areagp)
    (cond
      ((wcmatch txt_areagp "ACC")(setq fn "\\CableNo2"))
      ((wcmatch txt_areagp "ENG")(setq fn "\\CableNo1"))
      ((wcmatch txt_areagp "DECK")(setq fn "\\CableNo3"))
      (T (setq fn nil))
    )
  )
  (if (not (wcmatch *DCN_PATH "*.csv"))
    (progn
      (setq filename (SELCTMHFILE (cadddr (reverse txtlist))))
      (if (not filename)
	(setq filename (starct "\\" (getstring "which file?[CableNo1/CableNo2/CableNo3]") ".csv"))
      )
      (setq *DCN_PATH (strcat *DCN_PATH filename))
    )
  )
  (setq line_pos (1+ (CTG_getf_nline *DCN_PATH)))
  (prompt (strcat "data is input at the" (rtos line_pos 2 0) "'th line at standard file"))
  (setq str_ctmh_in (strcat "[" (cadr txtlist) "]" "," (caddr txtlist) "," (cadr (reverse txtlist)) "," (caddr (reverse txtlist))))
  (setq str_ctmh_in (strcat " ," str_ctmh_in))
  (setq DCN_fn (open *DCN_PATH "a"))
  (write-line str_ctmh_in DCN_fn)
  (close DCN_fn)
)

(defun CTG_getf_nline(files / tmplst x fn)
  ;(setq files(findfile files))
  (if files
    (progn
      (setq tmplst 0)
      (setq fn(open files "r"))
      (while (read-line fn)
        (setq tmplst (+ 1 tmplst))
      )
      (close fn)
      tmplst 
    )
    nil
  )
)

;(defun CTG_putf_strl (files instr / )
;  (setq i_line (1+ (CTG_getf_nline files)))
;  (setq fn (open files "a"))
;)

(defun CTG_getf_text (files line / fn text)
  (setq line(+ 1 line))
  (setq files (findfile files))
  (if files
    (progn
      (setq fn(open files "r"))
      (if (<= line (mc_getfile_line files))
        (progn
          (repeat line
            (setq text(read-line fn))
          )
          (close fn)
          text
        )
        nil
      )
    )
    nil
  )
)
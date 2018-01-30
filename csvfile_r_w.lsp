(defun CTG_getf_nline(files / tmplst x fn)
  (setq files(findfile files)
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
()
(defun CTG_putf_strl (files instr / )
  (setq i_line (1+ (CTG_getf_nline files)))
  (setq fn (open files "a"))
  
)
defun CTG_getf_text (files line / fn text)
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
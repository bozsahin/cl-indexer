(defun xfind_defs (fn)
  "This function returns a two element list. The first sublist is a list
   of definitions(names) and their positions in the input file fn. The second
   sublist is the body of the definitions plus top-level function calls,
   in the order of their position in the file."
  (with-open-file (ifile fn :direction :input )
    (do ((defs '(defun defmacro defconstant defstruct defvar defsetf 
                       defparameter deftype))
         (deflist nil)
         (body nil)
         (token (read ifile nil 'finito) (read ifile nil 'finito))
         (pos 1 (+ pos 1)))
        ((equal token 'finito) (list deflist (reverse body)))
      (cond ((member (car token) defs)
             (cond ((listp (second token))  ; a define with options
                    (push (list (car (second token))  pos) deflist))
                   (t (push (list (second token)  pos ) deflist)))))
      (push (list token  pos ) body))))


(defun index2 (ifn ofn)
    "This function indexes the input file ifn and writes
   the result to file ofn (no loading)"
    (setq curtable *readtable*
        *readtable* (copy-readtable nil))  ; use standard CL readtable
    (let* ((defines (xfind_defs ifn))
         (index (sort (first defines) #'string-lessp :key #'car))
         (bodies (second defines))
         (d (multiple-value-list (decode-universal-time (get-universal-time)))))
        (with-open-file (ofile ofn :direction :output
                :if-exists :new-version)
            (format ofile "~%; File name~26,1T:~31,2T~A~%;" ofn)
            (format ofile
                "~%; Created at~26,1T:~31,2T~D/~D/~D ~2,'0D:~2,'0D:~2,'0D by CLX~%;"
                (fifth d)(fourth d)(sixth d)(third d)(second d)(first d))
            (format ofile
                "~%; Indexed (source) file~26,1T:~31,2T~A~%;~%;" ifn)
            (format ofile
                "~%; INDEX OF DEFINITIONS:~%; --------------------------------------")
            (mapcar #'(lambda(entry)
                    (format ofile "~%; ~33,1,1,'.A~5D" (first entry)
                        (second entry)))
                index)
            (format ofile
                "~%;~%;~53,2T Common Lisp Indexer (CLX)")
            (format ofile
                "~%;~53,2T v 1.2, 1990, Cem Bozsahin")
            (format ofile "~%;~78,1,1,'-A" " ")
            (mapcar #'(lambda(body)
                    (format ofile "~%~%;~35,2T-----~%;~37,2T~D~%;~35,2T-----~%"
                        (second body))
                    (pprint (first body) ofile))
                bodies)
            (format ofile "~%")
            )
        (setq *readtable* curtable))  ; reset readtable back to original form
    ofn)

(defun index (ifn ofn)
  (setq _case *print-case*
        *print-case* :downcase)
  (cond ((equal ifn ofn) (princ "clx: can't overwrite source file.")
          (terpri))
    ((equal (probe-file ifn) nil)
      (princ "clx: can't open source file.") (terpri))
     (t (index2 ifn ofn)))
  (setq *print-case* _case)
  (princ "done.")(terpri))


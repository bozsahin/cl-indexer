;; model specific Eve code to support CCGlab model
;; - load after ccglab  -cem bozsahin Ankara 2017

(defparameter *bound-vars* nil) ; list of lambda-bound variables in an lf

(defun cat (p)
  (first p))

(defun token (p)
  (second p))

(defun load-transformer/eve-lf ()
  "LALR grammar for transforming Eve's dependency structures to ccglab's LF structures"
;; LALR parser demands lexical insertion by a pre-terminal for every terminal
;; (i.e. do not use constants in the RHSs of lalr rules)
;;  NB: We must have ID tag in 'lexforms' although there is nothing with that tag in the lexicon!
;; mk-x functions are lambda term makers from ccglab.
  (defparameter grammar 
    '((start --> body               #'(lambda (body) (identity body)))
      (start --> lterm              #'(lambda (lterm) (identity lterm)))
      (lterm --> LAM var US LCB typ RCB VALDOT lbody
	                            #'(lambda (LAM var US LCB typ RCB VALDOT lbody)
					(progn (push var *bound-vars*)
					       (mk-l (mk-v var) lbody))))
      (lbody --> lterm              #'(lambda (lterm) (identity lterm)))
      (lbody --> body               #'(lambda (body) (identity body)))
      (body  --> pred LP args RP    #'(lambda (pred LP args RP)(mk-a pred args)))
      (body  --> pred               #'(lambda (pred)(identity pred)))
      (args  --> args COM arg       #'(lambda (args COM arg)(mk-a arg args)))
      (args  --> arg                #'(lambda (arg)(identity arg)))
      (arg   --> var                #'(lambda (var)(mk-v var)))
      (arg   --> start              #'(lambda (start)(identity start)))
      (pred  --> ID                 #'(lambda (ID)(token ID)))
      (typ   --> ID                 #'(lambda (ID)(token ID)))
      (var   --> DOL ID             #'(lambda (DOL ID)(token ID)))
      ))
  (defparameter lexforms '(ID COM LCB RCB        ; all token types must be here, plus ID (see parse function in ccglab)
				 VALDOT US 
				 LP RP DOL LAM))
  (defparameter lexicon '((|.| VALDOT)
			  (|,| COM)
	                  (|)| RP)
			  (|(| LP)
			  (|{| LCB)
			  (|}| RCB)
			  (|_| US)
			  (|$| DOL)
			  (LAMBDA LAM)
			  (* *)        ; this is for lalrparser.lisp's end of input
			  ))
  ;; if you change the end-marker, change its hardcopy above in lexicon above as well.
  ;; (because LALR parser does not evaluate its lexicon symbols---sorry.)
  (defparameter *ENDMARKER* '*)
  ) ; of transformer/eve-lf

(defun make-transformer/eve-lf ()
  (load-transformer/eve-lf)
  (make-lalrparser)) ; from ccglab

(defun eve-pairs (fn &optional (debug nil))
  "takes list of pairs prepared by bash from file fn and returns supervision-ready pairs"
  (make-transformer/eve-lf) ; change the LALR grammar
  (let ((nl nil)
	(eve-pairs (with-open-file (s fn :direction :input :if-does-not-exist :error) (read s))))
    (dolist (s-lf eve-pairs)(progn (setf *bound-vars* nil) 
				   (push (list (first s-lf) (parse/2 (second s-lf))) nl)
				   (and debug (pprint (list s-lf *bound-vars*)))))
    (make-transformer/ccg)  ; reset LALR parser for general use in CCGlab
    nl))

(defun try-lambdas (pairs)
  "assuming Davidsonian variables are reversed in LF by the grammar above, 
  they are in functor position: reduce those"
  (let ((ps nil))
    (dolist (pair pairs)
      (cond ((is-l (sup-lf pair)) 
	     (let* ((term1 (beta (mk-a (sup-lf pair) (mk-l 'x 'x))))
		    (term2 (mk-a (sup-lf pair) (mk-l 'x 'x))))
	       (if (equal term1 term2) 
		 (push pair ps)
		 (push (list (sup-sentence pair) term1) ps))))
	    ((is-v (sup-lf pair)) (push (list (sup-sentence pair) (list (sup-lf pair))) ps))
	    (t (push pair ps))))
    ps))

(defun mk-list (o)
  (if (listp o) o (list o)))

(defun curry (lf &optional (res nil))
  (cond ((null lf) res)
	((is-a lf) (let* ((f (a-get-f lf))
			  (y (a-get-a lf))
			  (a (if (is-v y) (list y) y)))
		     (curry (rest a) (cons (mk-a f (first a)) res))))
	(t (append res (mk-list lf)))))

(defun eve-lf-steps (fn)
  "step by step lf-looking objects we get. Yoda"
  ;; as far as we can tell there are at most 2 davidsonian variables per LF in Eve
  (let ((pairs (try-lambdas (try-lambdas (eve-pairs fn))))  
	(newp nil))
    (dolist (np pairs)
      (push (list (sup-sentence np) (curry (sup-lf np))) newp))
    newp))


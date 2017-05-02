
; File name               :    eve.lsp
;
; Created at              :    5/2/2017 14:57:23 by CLX
;
; Indexed (source) file   :    eve.lisp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; *bound-vars*.....................    1
; cat..............................    2
; curry............................    9
; eve-lf-steps.....................   10
; eve-pairs........................    6
; load-transformer/eve-lf..........    4
; make-transformer/eve-lf..........    5
; mk-list..........................    8
; token............................    3
; try-lambdas......................    7
;
;                                                     Common Lisp Indexer (CLX)
;                                                     v 1.2, 1990, Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defparameter *bound-vars* nil)

;                                  -----
;                                    2
;                                  -----

(defun cat (p) (first p))

;                                  -----
;                                    3
;                                  -----

(defun token (p) (second p))

;                                  -----
;                                    4
;                                  -----

(defun load-transformer/eve-lf ()
  "LALR grammar for transforming Eve's dependency structures to ccglab's LF structures"
  (defparameter grammar
    '((start --> body #'(lambda (body) (identity body)))
      (start --> lterm #'(lambda (lterm) (identity lterm)))
      (lterm --> lam var us lcb typ rcb valdot lbody
       #'(lambda (lam var us lcb typ rcb valdot lbody)
           (progn (push var *bound-vars*) (mk-l (mk-v var) lbody))))
      (lbody --> lterm #'(lambda (lterm) (identity lterm)))
      (lbody --> body #'(lambda (body) (identity body)))
      (body --> pred lp domain com range rp
       #'(lambda (pred lp domain com range rp) (mk-a pred range)))
      (body --> pred lp args rp #'(lambda (pred lp args rp) (mk-a pred args)))
      (body --> pred #'(lambda (pred) (identity pred)))
      (domain--> var #'(lambda (var) (identity var)))
      (range --> pred lp var rp #'(lambda (pred lp var rp) (identity pred)))
      (args --> args com arg #'(lambda (args com arg) (mk-a arg args)))
      (args --> arg #'(lambda (arg) (identity arg)))
      (arg --> var #'(lambda (var) (mk-v var)))
      (arg --> start #'(lambda (start) (identity start)))
      (pred --> id #'(lambda (id) (token id)))
      (typ --> id #'(lambda (id) (token id)))
      (var --> dol id #'(lambda (dol id) (token id)))))
  (defparameter lexforms '(id com lcb rcb valdot us lp rp dol lam))
  (defparameter lexicon
    '((|.| valdot) (|,| com) (|)| rp) (|(| lp) ({ lcb) (} rcb) (_ us) ($ dol)
      (lambda lam) (* *)))
  (defparameter *endmarker* '*))

;                                  -----
;                                    5
;                                  -----

(defun make-transformer/eve-lf () (load-transformer/eve-lf) (make-lalrparser))

;                                  -----
;                                    6
;                                  -----

(defun eve-pairs (fn &optional (debug nil))
  "takes list of pairs prepared by bash from file fn and returns supervision-ready pairs"
  (make-transformer/eve-lf)
  (let ((nl nil)
        (eve-pairs
         (with-open-file (s fn :direction :input :if-does-not-exist :error)
           (read s))))
    (dolist (s-lf eve-pairs)
      (progn
       (setf *bound-vars* nil)
       (push (list (first s-lf) (parse/2 (second s-lf))) nl)
       (and debug (pprint (list s-lf *bound-vars*)))))
    (make-transformer/ccg)
    nl))

;                                  -----
;                                    7
;                                  -----

(defun try-lambdas (pairs)
  "assuming Davidsonian variables are reversed in LF by the grammar above, 
  they are in functor position: reduce those"
  (let ((ps nil))
    (dolist (pair pairs)
      (cond
       ((is-l (sup-lf pair))
        (let* ((term1 (beta (mk-a (sup-lf pair) (mk-l 'x 'x))))
               (term2 (mk-a (sup-lf pair) (mk-l 'x 'x))))
          (if (equal term1 term2)
              (push pair ps)
              (push (list (sup-sentence pair) term1) ps))))
       ((is-v (sup-lf pair))
        (push (list (sup-sentence pair) (list (sup-lf pair))) ps))
       (t (push pair ps))))
    ps))

;                                  -----
;                                    8
;                                  -----

(defun mk-list (o)
  (if (listp o)
      o
      (list o)))

;                                  -----
;                                    9
;                                  -----

(defun curry (lf &optional (res nil))
  (cond ((null lf) res)
        ((is-a lf)
         (let* ((f (a-get-f lf))
                (y (a-get-a lf))
                (a
                 (if (is-v y)
                     (list y)
                     y)))
           (curry (rest a) (cons (mk-a f (first a)) res))))
        (t (append res (mk-list lf)))))

;                                  -----
;                                    10
;                                  -----

(defun eve-lf-steps (fn)
  "step by step lf-looking objects we get. Yoda"
  (let ((pairs (try-lambdas (try-lambdas (eve-pairs fn)))) (newp nil))
    (dolist (np pairs)
      (push (list (sup-sentence np) (curry (sup-lf np))) newp))
    newp))

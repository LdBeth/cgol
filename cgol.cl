;;; $Header: /home/yew/yew5/users/phelps/cs/283/cgol/RCS/cgol.cl,v 1.6 1992/12/17 00:46:39 phelps Exp $
;;;-*-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From pratt@Shasta@Sumex-Aim  Sat Dec 19 20:04:27 1981
;;;                                                                      ;;;
;;;	Based on a theory of parsing presented in:                       ;;;
;;;                                                                      ;;;
;;;	    Pratt, Vaughan R., ``Top Down Operator Precedence,''         ;;;
;;;	    ACM Symposium on Principles of Programming Languages         ;;;
;;;	    Boston, MA; October, 1973.                                   ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp conversion by T.A. Phelps, November-December 1992

;;; The following terms may be useful in deciphering this code:
;;;
;;; NUD -- NUll left Denotation (op has nothing to its left (prefix))
;;; LED -- LEft Denotation	(op has something to left (postfix or infix))
;;;
;;; LBP -- Left Binding Power  (the stickiness to the left)
;;; RBP -- Right Binding Power (the stickiness to the right)

;;; Note: This file defines and uses a lot of syntax, therefore
;;;       there are both boostrapping and readability problems.
;;;       A classic problem with incremental re-write macrology.
;;;         - George Carrette.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; refer to cgol.cg for fully commented language definition ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTES
;;;  1. use table and iterate through to assign lbp, rbp
;;;     (or maybe macro for defprop, putprop)

;; how 'bout a macro for (defun funname...) followed by (setf (get 'fn-name 'nud) #'fn-name)
;; if use special setf then could I have multiple pairs for single setf?


;;;
;;; syntax
;;;


;; collect used properties names in one place
;; using macros because don't want arguments evaluated (as would be for function)

(defmacro defnud (fn-name fn-lambda)
  `(setf (get ,fn-name 'nud) ,fn-lambda))
(defmacro defled (fn-name fn-lambda)
  `(setf (get ,fn-name 'led) ,fn-lambda))
(defmacro deflbp (fn-name fn-lbp)
  `(setf (get ,fn-name 'lbp) ,fn-lbp))
(defmacro defrbp (fn-name fn-rbp)
  `(setf (get ,fn-name 'rbp) ,fn-rbp))
(defmacro defsf (fn-name fn-sf)
  `(setf (get ,fn-name 'storeform) ,fn-sf))

;; more readable versions

(defvar semicolon '|;|)
(defvar comma '|,|)
(defvar lparen '|(| )
(defvar rparen '|)|)
(defvar squote '|'|)
(defvar bslash '\\ )
(defvar vbar '\| )
(setq syntax-needed nil)	; for now, just SHOW translation (no evals)


;;;
;;; CGOL worker functions
;;;

;; special (i.e., dynamic) variables are bad, bad!
(eval-when (compile load)
(proclaim '(special CIBASE token stringnud syntax-needed DRBP FUN DENTYPE ISFUN SILENCE DEFBP
		  IVARS WHENVAR RESULT BODY nudl ledl lbpl cnud cled clbp LANGUAGE_ALIST
		  ARITHMETIC_ALIST)))


(defun advance nil
  (setq stringnud nil)
  (setq token (cgoltoken))
  (if (stringp token)
      (setq stringnud #'ret-tok token (intern (string-upcase token))))
  (if parser_debug (format t "=~a= " token))	; let's see the tokens
  (if scripting (format t "~a " token))
  token)
(defnud 'advance #'advance)

(defun verify (den) (cond (den (advance) den)))


(defun nuderr nil 
  (cond ((and (getden lbpl) nil (null (fboundp token)))	;led without a left
	 (cgolerr " missing preceding expression" 2 t))
	(t (let ((op token) (tp (peek-char))) ; otherwise return symbol as is.
	     (advance)
	     (list 'lambda nil
		   (list 'quote
			 (cond ((and (fboundp op)	; unary Lisp function
				      ;(member tp '(9 13 32))	;tab, return, space
				      (or stringnud
					  (and (getden nudl) (not (equal token lparen)))
					  (not (getden lbpl))))
;;	     (break "op=~a, tp=~c" op tp)
;;				(print "boy-o-boy")
				 (list op (parse (or (get op 'rbp) 25))))
				(t op))))))))
(defnud 'nuderr #'nuderr)




(defun lederr nil 
  (cgolerr  " is not an operator with a left argument" 2 t))
(defnud 'lederr #'lederr)


;; get denotation of current token, class indl, for current language (with inheritance)

(defun getden (indl)
   (and indl
	(or (and (symbolp token) (get token (car indl)))
	    (getden (cdr indl)))))

;;(defun (getden nud) nil (list (prog2 nil 'getden) (prog2 nil (parse 25))))
;;(setf (get 'getden 'nud) #'getden)	;; what about the (parse 25) stuff?
;;(defnud 'getden #'(lambda () (getden) (parse 25)))
(defnud 'getden #'getden)

;; extract the function on the nud property

(defun nud nil 
  (or (verify (or stringnud
		  (cond ((numberp token) (list 'lambda nil token)) (t (getden nudl)))))
      (nuderr)))
(defnud 'nud #'nud)


;; extract the function in the led property

(defun led nil (or (verify (getden ledl)) (lederr)))
(defnud 'led #'led)


;;; entry point from cgolread

(defun parse (rbp) 
  (do ((translation (funcall (nud)) (funcall (led) translation))) 
      ((not (< rbp (or (getden lbpl) 0))) translation) 
   nil))
;;(defun (parse nud) nil (list (prog2 nil 'parse) (prog2 nil (parse 25))))
;;(setf (get 'parse 'nud) #'parse)
(defnud 'parse #'parse)

(deflbp '$ -1)



;; ??? (DEFUN CGOL FEXPR (A) (CGOL-ENTER A) NIL)


(defnud 'cgolEXIT #'cgol-exit)


;-- jkf hack to allow =exit$ to work
;; --tap change this
;;(defun (exit nud) nil (list (prog2 nil 'exit)))

;;;
;;; speak-learn-forget development tools
;;;


(defun speak (x) 
  ((lambda (lang) 
     (cond (lang (setq lang (cdr lang)))
	   (t (cgolerr (cat x '| is an unknown language|) 3 t)))
     (setq nudl (cons (car lang) nudl))
     (setq ledl (cons (cadr lang) ledl))
     (setq lbpl (cons (caddr lang) lbpl))
     nil)
   (assoc x language_alist)))

(defun forget nil 
  (and (cdr nudl)
       (progn (setq nudl (cdr nudl)) (setq ledl (cdr ledl)) (setq lbpl (cdr lbpl))))
  nil)
(defnud 'forget #'forget)


(defun resetlanguage nil 
  (setq nudl '(nud))
  (setq ledl '(led))
  (setq lbpl '(lbp))
  (setq cnud 'nud)
  (setq cled 'led)
  (setq clbp 'lbp)
  nil)
(defnud 'resetlanguage #'resetlanguage)



;;(defun learn (x) 
;;  ((lambda (lang) 
;;     (cond (lang (setq lang (cdr lang)))
;;	   (t (setq lang (list (cat x 'nud) (cat x 'led) (cat x 'lbp)))
;;	      (setq language_alist (cons (cons x lang) language_alist))))
;;     (setq cnud (car lang)
;;     (setq cled (cadr lang))
;;     (setq clbp (caddr lang))
;;     `(or (assoc ',x language_alist) (push '(,x |`,\|| . lang) language_alist)))
;;   (assoc x language_alist))))



;;;
;;; auxiliary metalanguage functions
;;;


(defnud 'right		#'(lambda () (list 'parse drbp)))
(defnud 'rightlist	#'(lambda () (list 'parselist drbp comma)))
(defnud 'rightrep	#'(lambda () (list 'parselist drbp (list 'quote fun))))


(defun deffix (dentype isfun fun dlbp drbp) 
  ((lambda (form) 
     (cond (dlbp (setq form (list 'progn
				  ''compile
				  form
				  (list 'defprop fun dlbp clbp)))))
     (cond (syntax-needed (eval form)))
     form)
   (cons 'defun
	 (cons (list fun dentype)
	       (cons (cond ((equal dentype cled) '(left)))
		     (progn (advance) (deprognify (parse 0))))))))


(defnud 'nilfix #'(lambda () (deffix cnud 'isn token nil nil)))
(defnud 'prefix #'(lambda () (deffix cnud 'isp token nil (advance))))
(defnud 'suffix #'(lambda () (deffix cled 'iss token (advance) nil)))
(defnud 'infix #'(lambda () (deffix cled 'isi token (advance) token)))
;;(DEFUN (INFIXR NUD) NIL (DEFFIX CLED 'ISI TOKEN (ADVANCE) (DIFFERENCE TOKEN 1.)))
;; rjf 7 Jan 93
;;(defnud 'infixr #'(lambda () (deffix cled 'isi token (advance) problems)))
(defnud 'infixr #'(lambda () (deffix cled 'isi token (advance) (problems))))
(defnud 'infixd #'(lambda () (deffix cled 'isi token (advance) (advance))))
(defnud 'infixm #'(lambda () (deffix cled 'ism token (advance) token)))


;; mark delimiters as having 0 rbp, clbp lbp
(defnud 'delim
  #'(lambda ()
      ((lambda (form) (cond (syntax-needed (eval form))) form)
       (cons 'progn (mapcar #'(lambda (i) (list 'defprop i 0 clbp)) (getvarlist))))))


(defnud 'is
  #'(lambda ()
      (cons isfun	;isfun dynamic variable
	    (append (cond ((equal dentype cled) '(left)))
		    (list (parse 25))
		    (cond (drbp (list drbp)))
		    (cond ((equal isfun 'ism) (list (list 'quote fun))))))))

(defun isn (fcn) (list fcn))
(defun iss (left fcn) (list fcn left))
(defun isp (fcn rb) (list fcn (parse rb)))
(defun isi (left fcn rb) (list fcn left (parse rb)))
(defun ism (left fcn rb cont) (cons fcn (cons left (parselist rb cont))))


(defun check (del)   ; look for particular char or member of a list of chars
  (cond ((or (equal token del) (and (not (atom del)) (member token del))) (advance))
	(t (cgolerr (cat '|missing | del '| inserted before | token) 0 nil))))

;;rjf.  huh????
;;(defnud 'check #'(lambda () ((check) (parse 25))))

(defnud 'check #'(lambda () (check (parse 25))))

(defun cat (&rest n)  (apply 'concatenate 'string n))

(defun parselist (rb cont)
  (cons (parse rb) (cond ((eq token cont) (advance) (parselist rb cont)))))

(defun getvarlist nil 
  (cond ((or (not (equal token semicolon)) stringnud)
	 (cons (prog1 token (advance))
	       (cond ((equal token comma) (advance) (getvarlist)))))))
(defnud 'getvarlist #'getvarlist)


;; what the usefulness of this?
;;(DEFUN GETTOKENS NIL 
;;  (COND ((NOT (MEMBER TOKEN '(|)| ] \' || /;)))
;;	 (CONS (PROG2 NIL TOKEN (ADVANCE)) (GETTOKENS)))))
;;(DEFUN (GETTOKENS NUD) NIL (LIST (PROG2 NIL 'GETTOKENS)))


;; (progn x) => (x)
;; otherwise x => (x)

(defun deprognify (x) 
  (cond ((and (not (atom x)) (equal (car x) 'progn)) (cdr x)) (t (list x))))

;; (not x) => (x)
;; (x) => (not x)

(defun notify (x) 
  (and (not (equal x t))
       (cond ((and (not (atom x)) (equal (car x) 'not)) (cadr x))
	     (t (list 'not x)))))
;;(defun (notify nud) nil (list (prog2 nil 'notify) (prog2 nil (parse 25))))
(defnud 'notify #'(lambda () (list 'notify (parse 25))))


;; (x) => (or x)

(defun orify (x) 
  (and x (cond ((and (not (atom x)) (null (cdr x))) (car x)) (t (cons 'or x)))))
(defnud 'orify #'(lambda () (list 'orify (parse 25))))


;; ??? (DEFUN LITERAL FEXPR (X) (MAPC #'(LAMBDA (I) (SET I I)) X))


;; this could be hacked to be faster or cleverer
;; --tap  and less convoluted

(defun define nil 
  (let (fun ttype argts code instr lb rb form body)

	;; nud get function name
        ;; form - complete return form which defines operator
        ;; code - returned from nud/led
        ;; instr - parameter/delimiter hunks

	(cond ((or stringnud (equal (peek-char) 40))	;name is string or char
	       (setq code nil)
	       (setq instr `((prog1 ',token)))
	       (setq ttype cnud))
	;; led get first argument
	      (t (setq argts (list token))	; this is first argument
		 (advance)
		 (setq code `(',token))		; operator name
		 (setq instr '((prog1 left)))
		 (setq ttype cled)))
	(setq fun token)	; for error handling
	(advance)
;;(format t "fun=~a" fun)


;;   (format t "~%code=~a~%instr=~a" code instr)
	;; nud get arguments (a comma-separated list--no delimiters)
	(cond ((and (equal token lparen) (not stringnud))
	       (advance)
	       (setq argts (cond ((not (equal token rparen)) (getvarlist))))
	       (setq code nil)		; use assumed form of operator
	       (setq instr nil)
	       (check rparen))

	;; led - could be postfix or infix operator
	      ;; invariant: instr subsumes all following delimiters, returning the parameter
	      (t (do nil ((not (or (not (or (equal token semicolon) (equal token comma))) stringnud)))
;;   (format t "~%code=~a~%instr=~a" code instr)
		  (do nil ((not stringnud))
		    ;; delimiters -- check them and give them a 0 lbp
		    (setq instr (append instr `((check ',token))))
		    (setq form (append form `((deflbp ',token 0))))
;;   (format t "~%   code=~a~%   instr=~a" code instr)
		    (advance))
		   (setq code (append code instr))	; consolidate this param+delims
;;   (format t "~%   d code=~a~%   d instr=~a" code instr)
		   (cond ((and (or (equal token semicolon) (equal token comma)) (not stringnud))
			  (setq instr nil))	; for cleanliness sake
			 (t
			  (setq instr '((prog1 (parse 'rbp))))	;grab parameter, maintain invariant
			  (setq argts (append argts (list token)))
;;   (format t "~%   t code=~a~%   t instr=~a~%   t argts=~a" code instr argts)
			  (advance))))))
;;   (format t "~%code=~a~%instr=~a" code instr)
	   (setq code (append code instr))	; finish up loops


	;; get (optional) binding powers
	(setq lb (cond ((equal ttype cled)  ;nuds don't have lbp's
			(cond ((equal token comma) (advance) (eval (parse 1)))
			      (t defbp)))))
	(setq rb (cond ((equal token comma) (advance) (eval (parse 1)))
		       (t (or lb defbp))))
	(setq code (subst rb 'rbp code))	; fix up rbp placeholder with real value
	(check semicolon)
;;   (format t "~%code=~a~%instr=~a" code instr)



	;; make nud/led property (invocation function)
	;; (otherwise assumed a nud)

	(cond (code
	    (setq form (append
	      (list 'progn 
	       (if (equal ttype cnud)
		   `(defnud ',fun #'(lambda () ,code))
		   `(defled ',fun #'(lambda (left) ,code)))
	       (if lb `(deflbp ',fun ,lb)))
	      form))
	    ))
;;(format t "~%test=~a ~%     ~a" `(bozoitry ,@code) `(bozoitry '(,fun ,argts tex ,@code)))

	;; get body
	(cond ((not (equal token '$))
	       (setq form (append form `((defun ,fun ,argts ,@(deprognify (parse 0))))))))
	

	(setq form (cond (code form) (t (car form))))
	form))
	

(defnud 'define #'define)


;; ??? (DEFUN DEFTOK FEXPR (A) (MAPC #'PUTTOK A))

;;(defun (newtok nud) nil 
;;  ((lambda (form) (cond (syntax-needed (eval form))) form)
;;   (cons 'deftok (getvarlist))))
(defnud 'newtok
  #'(lambda ()
      ((lambda (form) (cond (syntax-needed (eval form))) form)
	 (cons 'deftok (getvarlist)))))


;; function call

(defled lparen	;function call
  #'(lambda (left)	; left is function name
      (prog1 (cons left (cond ((not (equal token rparen)) (parselist 0 comma))))  ;get list of actuals
	(check rparen))))
(defnud lparen #'(lambda () (prog1 (parse 0) (check rparen))))  ; (a) => a
(deflbp rparen 0)
(deflbp lparen 30)

(deflbp comma 0)


(defled '{	; this is an odd one
    #'(lambda (left) (prog1 (cons 'apply (cons (list 'function left) (parselist 0 comma))))
	      (check '})))
(deflbp '{ 30)
(deflbp '} 0)

(defnud '[
  #'(lambda ()
      (prog1
	  (cond ((not (equal token ']))	; if not empty list
		 ;; |)| makes circular list
		 ((lambda (a) (cond ((equal token rparen) (list 'circ a)) (t a)))
		  (cons 'list (parselist 0 comma)))))
	(check '(] |)|)))))
(defun circ (x) (prog1 x (rplacd (last x) x)))
(deflbp '] 0)


;;(DEFUN ([ LED) (LEFT) 
;;  (PROG2 NIL
;;	 (COND ((EQUAL TOKEN '{)
;;		(PROG2 NIL
;;		       (PROGN (ADVANCE)
;;			      (SUBLIS (LIST (CONS 'A LEFT) (CONS 'B (PARSE 0)))
;;				      '(APPLY #'MAPCAR (CONS #'A B))))
;;		       (CHECK '})))
;;	       (T (CONS 'MAPCAR
;;			(CONS (LIST 'FUNCTION LEFT) (PARSELIST 0 '/,)))))
;;	 (CHECK '])))
(defled '[
    #'(lambda ()
	(prog1
	    (cond ((equal token '{)	; an odd bird
		   (prog1
		       (progn (advance)
			      (sublis (list (cons 'a left) (cons 'b (parse 0)))
				      '(apply #'mapcar (cons #'a b))))
		     (check '})))
		  (t (cons 'mapcar	; a[b,c,...,z] => (mapcar (function a) b c ... z)
			   (cons (list 'function left) (parselist 0 comma)))))
	  (check ']))))
(deflbp '[ 30)


(defnud 'oct
  #'(lambda () (prog1 ((lambda (cibase) (check lparen) (parse 0)) 8) (check rparen))))

(defnud squote
  #'(lambda () (prog1 (isp 'quote 0) (check squote))))
(deflbp squote 0)


(defnud '=  #'(lambda () (eval (parse 25))))


(defnud bslash
  #'(lambda ()
      (prog1
	 (list 'lambda (prog1 (getvarlist) (check semicolon)) (deprognify (parse 0)))
	 (cond ((equal token rparen) (advance))))))
(deflbp bslash 0)


;; ??? do this later
(defnud 'let
     #'(lambda ()
  (prog (vars argts packflag) 
	(do nil 
	    ((member token '(semicolon in))) 
	 (setq vars (append vars (getvarlist))) 
	 (check '(be := =))
	 (setq argts (cons (cond ((equal token '{)
				  (list '&unp
					(prog2 nil
					       (progn (advance) (parse 0))
					       (progn (setq packflag t) (check '})))))
				 (t (parse 1.)))
			   argts))
	 (cond ((equal token comma) (advance))))
	(advance)
	(return
	 (cond
	  (packflag (setq argts
			  (reverse (mapcar #'(lambda (i) 
					       (cond ((equal (car i) '&unp) (cadr i))
						     (t (list 'list i))))
					   argts)))
		    (list 'apply
			  (list 'function
				(cons 'lambda (cons vars (deprognify (parse 0)))))
			  (cond ((equal (lengthx argts) 1.) (car argts))
				(t (cons 'append argts)))))
	  (t (cons (cons 'lambda (cons vars (deprognify (parse 0))))
		   (nreverse argts))))))))


(defnud 'prog
  #'(lambda ()
      (cons 'prog
	(cons (prog1 (getvarlist) (check semicolon)) (deprognify (parse 0))))))

(defnud 'new
  #'(lambda ()	;returns last
      (cons 'prog
	(cons (prog1 (getvarlist) (check semicolon))
	      ((lambda (x) ((lambda (y) (rplaca y (list 'return (car y))) x) (last x)))
	       (deprognify (parse 0)))))))

(defnud 'special  #'(lambda () (list 'proclaim `',(cons 'special (getvarlist)))))

(defnud 'literal  #'(lambda () (cons 'literal (parselist 1. comma))))

(defun cgolarray (x) ;fexpr (x) 
  (cond ((equal token lparen)
	 (prog1	(progn (advance)
		       (cons (car x)
			     (mapcar #'(lambda (y) (list 'sub1 y))
				     (parselist 0 comma))))
		(check rparen)))
	((equal token ':=) (advance) (list 'fillarray (car x) (parse 1)))
	(t (car x))))


(defnud 'array
  #'(lambda ()
  (COND
   ((MEMBER TOKEN '(|(| { [)) 'ARRAY)
   (T
    ((LAMBDA (NAMES) 
       ((LAMBDA (OLDNUDS) 
	  (PROG2
	   NIL
	   (PROGN
	    (MAPC 
	     #'(LAMBDA (NAME) 
		 (PUTPROP NAME (LIST 'LAMBDA NIL (LIST 'CGOLARRAY NAME)) CNUD))
	     NAMES)
	    (COND
	     ((EQUAL TOKEN lparen)
	      (ADVANCE)
	      ((LAMBDA (DIMS) 
		 (CHECK rparen)
		 ((LAMBDA (TYPE) 
		    ((LAMBDA (SOURCE) 
		       (COND
			((EQUAL TOKEN semicolon)
			 (ADVANCE)
			 (CONS
			  (CONS
			   'LAMBDA
			   (CONS
			    NAMES
			    (APPEND (COND (SOURCE (MAPCAR #'(LAMBDA (NAME) 
							      (LIST 'FILLARRAY
								    NAME
								    SOURCE))
							  NAMES)))
				    (DEPROGNIFY (PARSE 0)))))
			  (MAPCAR 
			   #'(LAMBDA (NAME) (CONS 'ARRAY (CONS NIL (CONS TYPE DIMS))))
			   NAMES)))
			(T
			 (CONS
			  'PROG2
			  (CONS
			   NIL
			   (CONS
			    (LIST 'QUOTE (CAR NAMES))
			    (MAPCAN 
			     #'(LAMBDA (NAME) 
				 (CONS (LIST 'DEFPROP
					     NAME
					     (GET NAME 'NUD)
					     'NUD)
				       (CONS (LIST 'SETQ
						   NAME
						   (CONS 'ARRAY
							 (CONS NIL (CONS TYPE DIMS))))
					     (COND (SOURCE (LIST (LIST 'FILLARRAY
								       NAME
								       SOURCE)))))))
			     NAMES)))))))
		     (COND ((MEMBER TOKEN '(:= =)) (ADVANCE) (PARSE 1.)))))
		  (COND ((MEMBER TOKEN '(FIXNUM FLONUM NIL T))
			 (PROG2 NIL TOKEN (ADVANCE)))
			(T T))))
	       (PARSELIST 0 comma)))
	     ((EQUAL TOKEN semicolon) (ADVANCE) (PARSE 0))))
	   (MAPC #'(LAMBDA (NAME OLDNUD) 
		     (COND (OLDNUD (PUTPROP NAME OLDNUD CNUD)) (T (REMPROP NAME CNUD))))
		 NAMES
		 OLDNUDS)))
	(MAPCAR #'(LAMBDA (NAME) (GET NAME CNUD)) NAMES)))
     (GETVARLIST))))))


;; uh oh, what does Common Lisp do for this?
;;(DEFUN (DIM NUD) NIL (LIST 'CDR (LIST 'ARRAYDIMS (PARSE 25))))
;; integrate this in with rewrite of array
(defnud 'dim  #'(lambda () (list 'cdr (list 'arraydims (parse 25)))))

(defrbp 'eval 1)

(defled semicolon  #'(lambda (left) (ism left 'progn 1 semicolon)))
(deflbp semicolon 1)
(defled '&  #'(lambda (left) (list 'prog1 left (parse 0))))
(deflbp '& 1)


(defnud 'if
  #'(lambda ()
      (cons 'cond
	    (cons (cons (parse 2) (progn (check 'then) (deprognify (parse 2))))
		  (cond ((eq token 'else)
			(advance)
			((lambda (x)
			   (cond ((and (not (atom x)) (equal (car x) 'cond)) (cdr x))  ;nested if's?
				  (t (list (cons t (deprognify x))))))	; no
			   (parse 2))))))))
(deflbp 'then 0)
(deflbp 'else 0)
(defrbp 'return 1)
(defrbp 'go 1)

(defnud 'while
  #'(lambda () (cons 'do
		     (cons nil
			   (cons (list (notify (parse 2)))
				 (progn (check 'do) (deprognify (parse 2))))))))

(defnud 'repeat
  #'(lambda ()
      (list 'do	nil
	(list (cons 'prog2
		    (append (deprognify (parse 2))
			    (deprognify (progn (check 'until) (parse 2)))))))))
(deflbp 'do 0)


;; ??? do this later
(defnud 'for
  #'(lambda ()
  (PROG (PARS ARGTS INON FCN BODY) 
	(SETQ PARS (LIST TOKEN))
	(SETQ INON (ADVANCE))
	(ADVANCE)
	(SETQ FCN (ASSOC INON
			 '((IN (DO MAPC) (COLLECT MAPCAR) (COALESCE MAPCAN))
			   (ON (DO MAP) (COLLECT MAPLIST) (COALESCE MAPCON)))))
	(COND (FCN (SETQ FCN (CDR FCN)))
	      (T (CGOLERR (CAT INON '| FOUND WHERE IN OR ON EXPECTED|) 2 T)))
	(SETQ ARGTS (LIST (PARSE 1)))
	(DO NIL 
	    ((NOT (EQ TOKEN comma))) 
	 (SETQ PARS (CONS (ADVANCE) PARS)) 
	 (ADVANCE)
	 (CHECK INON)
	 (SETQ ARGTS (CONS (PARSE 1) ARGTS)))
	(SETQ FCN (ASSOC TOKEN FCN))
	(COND (FCN (SETQ FCN (CADR FCN)))
	      (T (CGOLERR (CAT TOKEN '| FOUND WHERE DO, COLLECT OR COALESCE EXPECTED|)
			  2
			  T)))
	(ADVANCE)
	(SETQ ARGTS (NREVERSE ARGTS))
	(SETQ PARS (NREVERSE PARS))
	(SETQ BODY (PARSE 1.))
	(RETURN
	 (COND ((AND (EQUAL FCN 'MAPC)
		     (APPLY #'AND
			    (MAPCAR #'(LAMBDA (X)
					(and (not (atom x)) (EQUAL (CAR X) 'TO)))
				    ARGTS)))
		(CONS 'DO
		      (CONS (MAPCAR #'(LAMBDA (P A) 
					(LIST P
					      (CADR A)
					      (COND ((EQUAL (CADDDR A) 1.)
						     (LIST 'ADD1 P))
						    (T (LIST 'PLUS P (CADDDR A))))))
				    PARS
				    ARGTS)
			    (CONS (LIST (ORIFY (MAPCAR #'(LAMBDA (P A) 
							   (LIST 'GREATERP
								 P
								 (CADDR A)))
						       PARS
						       ARGTS)))
				  (DEPROGNIFY BODY)))))
	       (T (CONS FCN
			(CONS (LIST 'FUNCTION
				    (COND ((AND (EQUAL (CDR BODY) PARS) (ATOM (CAR BODY)))
					   (CAR BODY))
					  (T (LIST 'LAMBDA PARS BODY))))
			      ARGTS))))))))
(deflbp 'in 0)
(deflbp 'on 0)
(deflbp 'collect 0)
(deflbp 'coalesce 0)


;; ??? another bad boy (do later)
(DEFUN ITER NIL 
  (PROG (IVARS WHENVAR RESULT BODY IT) 
	(DO NIL 
	    ((NOT
	      (SETQ 
	       IT
	       (ASSOC TOKEN
		      '((FOR (SETQ IVARS (CONS (CONS TOKEN (COND ((EQUAL (ADVANCE) ':=) (CONS (PROGN (ADVANCE) (SETQ IT (PARSE 2.))) (COND ((EQUAL TOKEN '
																			   STEP) (LIST (COND ((EQUAL (ADVANCE) '
																								DITTO) (ADVANCE) IT) (T (PARSE 2.)))))))))) IVARS)))
			(WHEN (SETQ WHENVAR (PARSE 2.)))
			(UNTIL (SETQ WHENVAR (PARSE 2.)))
			(WHILE (SETQ WHENVAR (LIST 'NOT (PARSE 2.))))
			(RETURN (SETQ RESULT (PARSE 2.))) (DO (SETQ BODY (PARSE 2.)))))))) 
	 (ADVANCE) 
	 (EVAL (CADR IT)))
	(COND ((NOT (OR IVARS WHENVAR RESULT BODY)) (SETQ BODY (PARSE 2.))))
	(RETURN (APPEND (LIST 'DO (NREVERSE IVARS) (LIST WHENVAR RESULT))
			(COND ((AND (NOT (ATOM BODY)) (EQ (CAR BODY) 'PROGN))
			       (CDR BODY))
			      (T (NCONS BODY)))))))
(defnud 'iter #'iter)

(deflbp 'for 0)
(deflbp 'when 0)
(deflbp 'until 0)
(deflbp 'while 0)
(deflbp 'step 0)
(deflbp 'return 0)

(defled 'to
  #'(lambda (left)
      (cons 'to
	    (cons left
		  (cons (parse 18)
			(list (cond ((equal token 'by) (advance) (parse 18)) (t 1))))))))
(deflbp 'to 18)
(deflbp 'by 0)

(defun to (aa b c) 
  (cond ((> aa B) NIL)
	(t (prog (x) 
		 (return (prog1	(setq x (list aa))
				(do nil 
				    ((lessp b (setq aa (plus aa c)))) 
				 (setq x (cdr (rplacd x (list aa)))))))))))

(defled 'lotsof
  #'(lambda (left)
      (list 'do '*i left '(difference *i 1) '(not (> *i 0)) (parse 1))))
(deflbp 'lotsof 19)


;; ??? what to do about deftoks?  (DEFTOK /:=)

(defnud 'cgolprint #'(lambda () (list 'cgolprint (parse 1))))
(defnud 'cgolprin1 #'(lambda () (list 'cgolprin1 (parse 1))))


;; assignment--very important
(defled '|:=|
    #'(lambda (left)
      (cond ((atom left) (isi left 'setq 1))
	    ((eq (car left) 'get)
	     (list 'setf left (parse 1) (caddr left)))
;; fix these later
;;	    (t (format t "in :="))
	    
;;	    ((setq it (get (car left) 'storeform))
;;	     ((lambda (x) 
;;		(sublis (list (cons 'left (cadr left)) (cons 'right (parse 1))) x))
;;	      it))
	    ;;	    (t (isi left 'store 1))
	    )))

(deflbp '|:=| 25)

(defsf 'car '(rplaca left right))
(defsf 'cdr '(rplacd left right))
(defsf 'arg '(setarg left right))
(defsf 'plist '(setplist left right))
;;(defsf 'status '(sstatus left right))

;; what does this do? random maclisp stuff
;;(MAPC #'(LAMBDA (I) 
;;	  (PUTPROP I (SUBST I 'I '(LAMBDA NIL '(STATUS I))) 'NUD))
;;      '(TOPLEVEL BREAKLEVEL WHO2 WHO3 TTYSCAN TTYREAD TTYINT GCTIME))


(defled 'of #'(lambda (left) (list 'get (parse 25) left)))
(deflbp 'of 26)

(defled 'ofq #'(lambda (left) (list 'get (parse 25) (list 'quote left))))
(deflbp 'ofq 26)

(defrbp 'not 9)
(deflbp 'not 10)
(defled 'not #'(lambda (left) (list 'not (funcall (led) left))))

(defled 'and #'(lambda (left) (ism left 'and 8 'and)))
(deflbp 'and 8)

(defled 'or #'(lambda (left) (ism left 'or 7 'or)))
(deflbp 'or 7)


;;(DEFTOK =/#)
;;(DEFTOK =$)
;;(DEFTOK </#)
;;(DEFTOK >/#)
;;(DEFTOK <$)
;;(DEFTOK >$)
;;(DEFTOK <=)
;;(DEFTOK >=)


(defled '= #'(lambda (left) (isi left '= 10)))
(deflbp '= 10)

(defled 'ne #'(lambda (left) (not (isi left '= 10))))
(deflbp 'ne 10)

(defled 'eq #'(lambda (left) (isi left 'eq 10)))
(deflbp 'eq 10)

(defled '< #'(lambda (left) (ism left '< 10 '<)))
(deflbp '< 10)

(defled '> #'(lambda (left) (ism left '> 10 '>)))
(deflbp '> 10)

(defled '<= #'(lambda (left) (list 'not (isi left '> 10))))
(deflbp '<= 10)

(defled '>=  #'(lambda (left) (list 'not (isi left '< 10))))
(deflbp '>= 10)

;;(DEFUN (/| LED) (LEFT) 
;;  (LIST (ARITH 'ZEROP) (LIST (ARITH 'REMAINDER) (PARSE 10) LEFT)))
;; | messed up
(defled vbar
  #'(lambda (left)
      (list 'zerop (list 'rem (parse 10) left))))
;;(DEFPROP /| 10 LBP)
(deflbp vbar 10)


(defled 'isin  #'(lambda (left) (isi left 'member 10)))
(deflbp 'isin 10)
(defled 'isatom  #'(lambda (left) (iss left 'left)))
(deflbp 'isatom 10)
(defled 'isnum  #'(lambda (left) (iss left 'numberp)))
(deflbp 'isnum 10)
(defled 'exists  #'(lambda (left) (list 'setq 'it left)))
(deflbp 'exists 10)

(defrbp 'null 10)

;;(defled '\.  #'(lambda (left) (isi left 'cons 14)))
;;(deflbp '\. 15)
(defled '^  #'(lambda (left) (isi left 'cons 14)))
(deflbp '^ 15)
(defled '@  #'(lambda (left) (ism left 'append 15 '@)))
(deflbp '@ 15)


;; set operations.  many of these changed from control characters
;; ??? easy, but do later

;;(DEFUN ({ NUD) NIL 
;;  (PROG2 NIL
;;	 (CONS 'GATHER
;;	       (COND ((NOT (EQUAL TOKEN '})) (PARSELIST 0 '/,))))
;;	 (CHECK '})))
(setf (get '{ 'nud)
  #'(lambda ()
  (prog1
      (cons 'gather (cond ((not (equal token '})) (parselist 0 comma))))
    (check '}))))

;; ??? do these later
;;(DEFUN (|| LED) (LEFT) (ISM LEFT 'UNION 16. '||))
;;(DEFPROP || 16. LBP)
;;(DEFUN (/ LED) (LEFT) (ISM LEFT 'INTERSECT 16. '/))
;;(DEFPROP / 16. LBP)
;;(DEFUN (~ NUD) NIL (ISP 'SETDIFF 16.))
;;(DEFUN (~ LED) (LEFT) (ISM LEFT 'SETDIFF 16. '~))
;;(DEFPROP ~ 16. LBP)
;;(DEFUN (/ LED) (LEFT) (ISM LEFT 'ELEMENTP 10 '/))
;;(DEFPROP / 10 LBP)
;;(DEFUN (/ LED) (LEFT) (ISM LEFT 'SUBSETP 10 '/))
;;(DEFPROP / 10 LBP)
;;(MAPC #'(LAMBDA (U) 
;;	  (OR (FBOUNDP U) (PUTPROP U '((DSK LIBLSP) SETS FASL) 'AUTOLOAD)))
;;      '(GATHER UNION INTERSECT SETDIFF ELEMENTS ELEMENTP SUBSETP SYMDIFF CLEARSETS))
;;(IF (FBOUNDP '*LEXPR) (*LEXPR UNION INTERSECT SETDIFF SYMDIFF))


;;(defled '^  #'(lambda (left) (ism left 'cat 18 '^)))
;;(deflbp '^ 18)
(defled 'cat  #'(lambda (left) (ism left 'cat 18 'cat)))
(deflbp 'cat 18)


(defnud vbar  #'(lambda () (prog1 (isp 'abs 19) (check vbar))))


(defnud '+  #'(lambda () (cond ((member token '(|(| { [)) '+) (t (parse 20)))))
(defled '+  #'(lambda (left) (ism left '+ 20 '+)))
(deflbp '+ 20)

(defled '- #'(lambda (left) (ism left '- 20 '-)))
(deflbp '- 20)
(defnud '-  #'(lambda () (isp '- 20)))


(defled '*  #'(lambda (left) (ism left '* 21 '*)))
(deflbp '* 21)

;; ??? later
;;(DEFUN (// LED) (LEFT) 
;;  (LIST (ARITH 'QUOTIENT) LEFT (LIST (ARITH 'FLOAT) (PARSE 21.))))
;;(DEFPROP // 21. LBP)
;;(setf (get '/ 'lbp) 21)
(defled '/  #'(lambda (left) (ism left '/ 21 '/)))
(deflbp '/ 21)



;; ??? later
;;(DEFTOK |//:|)
;;(DEFUN (|//:| LED) (LEFT) (ISM LEFT (ARITH 'QUOTIENT) 21. '|//:|))
;;(DEFPROP |//:| 21. LBP)


;;(DEFUN (REM LED) (LEFT) (ISI LEFT (ARITH 'REMAINDER) 21.))
;;(DEFPROP REM 21. LBP)
(defled 'rem  #'(lambda (left) (isi left 'rem 21)))
(deflbp 'rem 21)

(defled 'mod  #'(lambda (left) (list 'mod left (parse 21))))
(deflbp 'mod 21)

(defled '**  #'(lambda (left) (isi left 'expt 21)))
(deflbp '** 22)



;; booleans done better!
;;  now names correspond to Common Lisp's

(defnud 'bitnot  #'(lambda () (list 'lognot (parse 21))))
(defled 'bitand  #'(lambda (left) (ism left 'logand 21 'logand)))
(deflbp 'bitand 22)
(defled 'bitor  #'(lambda (left) (ism left 'logior 20 'logor)))
(deflbp 'bitor 21)
(defled 'bitxor  #'(lambda (left) (ism left 'logxor 20 'logxor)))
(deflbp 'bitxor 21)
(defled 'bitshift  #'(lambda (left) (ism left 'ash 20 'ash)))
(deflbp 'bitshift 20)


(defnud 'log #'(lambda () (isi left 'log 1)))

(defrbp 'print 2)
(defrbp 'princ 2)
(defrbp 'prin1 2)
(defnud 'write
     #'(lambda () (subst (cons 'list (parselist 2 comma))
			 'x
			 '(progn (terpri) (mapc #'princ x) (princ '| |)))))
(defnud 'newline  #'(lambda () (isn 'terpri)))

;;(DEFUN (UREAD NUD) NIL (CONS 'UREAD (GETTOKENS)))
;;(DEFUN (UWRITE NUD) NIL (CONS 'UWRITE (GETTOKENS)))
;;(DEFUN (UFILE NUD) NIL (CONS 'UFILE (GETTOKENS)))
;;(DEFUN (LOAD NUD) NIL (CONS 'FASLOAD (GETTOKENS)))
;; no disk i/o for now


;;(setq silence -1)
(setq defbp 25)

(setq fun 'top-level)
(setq language_alist nil)
(setq arithmetic_alist nil)
;;(SSTATUS FEATURE CGOL)

(setq nudl '(nud))	;how about calling resetlanguage?
(setq ledl '(led))
(setq lbpl '(lbp))
(setq cnud 'nud)
(setq cled 'led)
(setq clbp 'lbp)

;; fix period (shared between numbers and cons).  maybe make constituent
;; I think this belongs with the parser
(initialize-multi-character-token-table "$|-+#&;)(*,'/:<=>@[\\]^`{|}~")
(set-macro-character #\. #'smash-token t *cgol-readtable*)
(setq syntax-needed t)








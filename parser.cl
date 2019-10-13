;;;From phelps@palm.CS.Berkeley.EDU Wed Dec 16 16:37:17 1992
;;; $Header: /home/yew/yew5/users/phelps/cs/283/cgol/RCS/parser.cl,v 1.3 1992/12/03 20:34:18 phelps Exp phelps $
;;;
;;; cgol.cl by V.A. Pratt
;;; largely rewritten, in Common Lisp, by T.A. Phelps, November-December 1992
;;; University of California at Berkeley
;;;

;;; gosh, this file is MUCH smaller than before

(defvar token nil)
;;(defvar ret-nud nil "The instance variable of a recycled closure")
;;(defun ret-nud () ret-nud)
(defvar stringnud nil)	; points to ret-nud FUNCTION if non-null
(defun ret-tok () token)

(defvar cibase 10.)
(defvar cgolerr nil "controls throws for eof condition")
(defvar parser_debug nil "if t show stream of tokens")
(defvar scripting nil "if t show stream of input tokens")
(defvar ctoken-table nil)	; badly done

(defvar fun 'TOP-LEVEL)	; for error handling
;;(defvar silence -1)	; ???


;;;
;;; PARSER - returns tokens
;;;

(defvar *lisp-readtable* (copy-readtable))
(defvar *cgol-readtable* (copy-readtable))

(defun lisp-read (&optional (stream *standard-input*))
  (let ((*readtable* *lisp-readtable*))
    (read stream)))

(defun cread (&optional (stream *standard-input*))
  (let ((*readtable* *cgol-readtable*))
    (read stream)))

(defun read-comment (stream character)
  (declare (ignore character))
  (let ((*readtable* *lisp-readtable*))
	(read-delimited-list #\% stream)
	(cread)))


(defun initialize-multi-character-token-table (string)
  (setq ctoken-table string)
  (every #'(lambda (c) (set-macro-character c #'smash-token nil *cgol-readtable*)) string))

(defun smash-token (stream c)
  (intern (coerce
	   (cfollow-tail c stream (subseq ctoken-table (1+ (position c ctoken-table))))
	   'string)))

(defun cfollow-tail (c stream table)
  ;; this way of recognizing tokens is taken from the original cgol,
  ;; is fast and easy and passes all tokens which are subtokens
  ;; of explicitly defined tokens.
  ;; [but it will pass erroneous multi-character tokens.  --tap]
  (let* ((c2 (char-upcase (peek-char nil stream)))
	 (posn (position c2 table)))
    (cons c
	  (if posn
	      (progn (read-char stream)
		     (cfollow-tail c2 stream (subseq table (1+ posn))))))))

(let ((*readtable* *cgol-readtable*))
  ; unchanged are: string, numbers, whitespace 
  (set-macro-character #\% #'read-comment nil)
  (set-macro-character #\! #'(lambda (s c) (declare (ignore c))
				     (lisp-read s)) nil)
  (set-syntax-from-char #\? #\\)	; escape character is '?'
  ; semicolon is now statement terminator--changed by initialize-multi-character-token-table
  )

(defun cgoltoken ()
  (cread))


;;--------------------------------------------------
;; yuckiness follows

;;; *** USER ENTRY POINT #1 ***
;;; read a CGOL expression,
;;; then call parse to convert it to Common Lisp

;; replace most of this with (parse -1)
;; add error handling later

;;; rjf ??
(defvar eofm nil)

(DEFUN CGOLREAD (&REST READ-ARGS)
  (let (stream eofm)	;Norvig, in his GHWB impression, says &AUX is "bad! bad!"
    ; dispatch on first character, save rest in eofm
  (SETQ STREAM (or (CAR READ-ARGS) *standard-input*) ;;fix by meltsner 8Jan93
	EOFM (CDR READ-ARGS))
    (CATCH 'CGOLERR (toplevel-parse stream))))

(DEFUN TOPLEVEL-PARSE (*STANDARD-INPUT*
		       &AUX
;;		       ;; State variables.
;;		       (CGOLERR T) TOKEN STRINGNUD RET-NUD
		       ;;		       (FUN 'TOP-LEVEL)
		       parser-debug
;; may throw the eof marker here.
		       )
  (format t "~%cgol(1)> ")
  (let ((expr 'do-at-least-once) (show-syntax t))
    (setq cgolerr nil)
    (setq parser-debug nil)
    (setq scripting nil)
    (do ((ctr 2 (+ ctr 1)) (bozo (advance) (advance))) ((not expr) 'ok)
      (setq expr (parse -1))
      (cond ((eq expr 'eval) (setq syntax-needed (not syntax-needed)))
	    ((eq expr 'show) (setq show-syntax (not show-syntax)))
	    ((eq expr 'parser_debug) (setq parser-debug (not parser-debug)))
	    ((eq expr 'scripting) (setq scripting (not scripting)))
	    ((eq expr 'quit) (setq expr nil))
	    (t (if show-syntax (format t "~%  Lisp> ~a " expr))
	       (if syntax-needed (format t "~%  Value> ~a" (eval expr)))))
      (format t "~%cgol(~d)> " ctr)
      )))


;;  (COND ((EQ (ADVANCE) 'EsCaPe)	;should be escape character
;;	 ;; KLUDGE for old CGOL source files.
;;	 ''EsCaPe)
;;	(T
;;	 (SETQ CGOLERR NIL)
;;	 (PARSE -1)))

(defun cgolerr (message level fatalp)
  (declare (ignore level)) ;; someday, do something more sophisticated
  (cond ((and fatalp cgolerr)
	 (throw 'cgolerr eofm))
	(t
	 (error (concat "Error: " message " in " fun)))))



;; The problem of invoking CGOL over a whole stream is correctly solved by
;; pushing and popping a stack of read methods for a stream.
;; However, maclisp and the lisp machine provide special variables READ and READTABLE
;; for this.

(defvar cgol-rt-stack nil "holds stack of readtables")

(defun cgol-enter (ignore-it)
  (declare (ignore ignore-it))
  (push *readtable* cgol-rt-stack))

(defun cgol-exit ()
  (if (consp ctol-rt-stack)
      (setf *readtable* (pop cgol-rt-stack))))

(defun cgol () (cgolread))
(defun cg () (cgolread))  ;shorthand


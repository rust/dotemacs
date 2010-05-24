;;; PCE.el --- Primary Composition Engine
;;;                   

;; Author: MIYASHITA Hisashi <himi@li18nux.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(require 'bytecomp)
(require 'EIMIL)
(require 'iiimcf)
(require 'iiimp)

(defconst PCE-arglist-regexp
  (concat "\\(" EIMIL-symbol-regexp
	  "\\):\\(bool\\|number\\|prop\\|mtext\\|event\\)"))

;;; Private EIMIL slot usage
;; [<state-list> <unread-event-list> <main-program>]

;;;
;;; Setup/Initialization
;;;

(defun PCE-init-eobj (eobj)
  (EIMIL-set-private
   eobj
   (vector
    nil ; state-list
    nil ; current-state
    nil ; main-function
    ))
  ;; register internal variable.
  (let (sym)
    (setq sym (EIMIL-intern-symbol
	       eobj "nil"
	       'variable nil))
    (EIMIL-symbol-set-type sym 'bool)
    (set sym nil)
    (setq sym (EIMIL-intern-symbol
	       eobj "t"
	       'variable nil))
    (set sym t)
    (EIMIL-symbol-set-type sym 'bool)
    (setq sym (EIMIL-intern-symbol
	       eobj "curev"
	       'variable nil))
    (set sym nil)
    (EIMIL-symbol-set-type sym 'event)
    (setq sym (EIMIL-intern-symbol
	       eobj "mapval"
	       'variable nil))
    (set sym nil)
    ))


(defsubst PCE-set-main-function (eobj func)
  (aset (EIMIL-private eobj) 2 func))

(defsubst PCE-get-main-function (eobj)
  (aref (EIMIL-private eobj) 2))

(defsubst PCE-state-list (eobj)
  (aref (EIMIL-private eobj) 0))

(defsubst PCE-add-state (eobj label)
  (aset (EIMIL-private eobj) 0
	(cons label (aref (EIMIL-private eobj) 0))))

(defsubst PCE-set-current-state (eobj label)
  (aset (EIMIL-private eobj) 1 label))

(defsubst PCE-get-current-state (eobj)
  (aref (EIMIL-private eobj) 1))


;;;
;;; Hash
;;;

(defconst PCE-default-hash-table-size 53)
(defun PCE-make-hash-table ()
  (make-vector PCE-default-hash-table-size 0))

(defsubst PCE-hash-key-to-string (key)
  (cond ((stringp key) key)
	((numberp key) (number-to-string key))
	((eq (car key) 'char) (number-to-string (cdr key)))
	((eq (car key) 'key)
	 )
	(t (error "Invalid key:%S" key))))

(defun PCE-put-hash (hash key val)
  (setq key (PCE-hash-key-to-string key))
    (if (intern-soft key hash)
	(error "Duplicate hash key %S !" key))
    (set (intern key hash) val))

(defun PCE-get-hash (hash key)
  (symbol-value
   (intern-soft (PCE-hash-key-to-string key) hash)))

;; special arguemnt type:
;;  statements
;;  numbers
;;  select
;;; (<symbol> <ret. type> <gen. func.> [<ATTR-SPEC> <ARG1>...])
(defconst PCE-elements-alist
  '((go fc PCE-gen-go ((to . nmtoken)))
    (label fc PCE-gen-label ((name . nmtoken)))
    (next fc PCE-gen-next ((remove . ("true" "false"))) event)
    (forward fc PCE-gen-forward ((to . nmtoken)))
    (send fc PCE-gen-send ((to . nmtoken)) event)
    (undo undoc (default . PCE-exec-undo) nil)
    (mark-undo undoc PCE-gen-mark-undo nil)
    (try sth PCE-gen-try nil nil) ; statements, <catch>
    (throw sth PCE-gen-throw ((ex . nmtoken)))
    (while flow PCE-gen-while nil expression statements)
    (if flow PCE-gen-if nil nil) ; statements, <else>
    (select flow PCE-gen-select nil nil) ; (keycase|keymap)*, default
    (return flow PCE-gen-return nil expression)
    ;; bool
    (toggle-preedit bool (default . PCE-exec-toggle-preedit) nil bool)
    (toggle-lookup-choice bool (default . PCE-exec-toggle-lookup-choice) nil bool)
    (keyeventp bool (default . PCE-exec-keyeventp) nil event)
    (or bool (default . or) nil expressions)
    (and bool (default . and) nil expressions)
    (not bool (default . not) nil expression)
    (gt bool (default . >) nil number number)
    (lt bool (default . <) nil number number)
    (le bool (default . <=) nil number number)
    (ge bool (default . >=) nil number number)
    (eql bool (default . =) nil number number)
    ;; number
    (tblkeymaxsize number PCE-gen-tblkeymaxsize ((t . nmtoken)))
    (tblvalmaxsize number PCE-gen-tblvalmaxsize ((t . nmtoken)))
    (propsize number (default . PCE-exec-propsize) nil prop)
    (propmbeg number (default . PCE-exec-propmbeg) nil prop)
    (propmend number (default . PCE-exec-propmend) nil prop)
    (evval number (default . PCE-exec-evval) nil prop)
    (evmod number (default . PCE-exec-evmod) nil prop)
    (strlen number (default . length) nil mtext)
    (strcmp number (default . PCE-exec-strcmp) nil mtext mtext)
    (add number (default . +) nil numbers)
    (sub number (default . -) nil numbers)
    (mul number (default . *) nil numbers)
    (div number (default . /) nil numbers)
    (mod number (default . %) nil number number)
    (bor number (default . logior) nil numbers)
    (band number (default . logand) nil numbers)
    (bxor number (default . logxor) nil numbers)
    (UCSval number (default . PCE-exec-UCSval) nil char)
    ;; prop
    (makeprop prop PCE-gen-makeprop ((p . nmtoken)))
    (propadd prop (default . PCE-exec-propadd) nil prop expression)
    (propcopy prop (default . PCE-exec-propcopy) nil prop)
    (getmprop prop PCE-gen-getmprop ((p . nmtoken)) mtext number)
    (findmprop prop PCE-gen-findmprop ((p . nmtoken)) mtext number)
    ;; char
    (evchar char (default . PCE-exec-evchar) nil event)
    (strref char (default . aref) nil mtext number)
    (makechar char (default . PCE-exec-makechar) nil number)
    ;; mtext
    (interact mtext PCE-gen-interact ((op . nmtoken)))
    (match mtext PCE-gen-match ((p . nmtoken)) mtext)
    (evtype mtext (default . PCE-exec-evtype) nil event)
    (evmtext mtext (default . PCE-exec-evmtext) nil event)
    (concat mtext PCE-gen-concat nil nil) ; mtext or char
    (substr mtext PCE-gen-substr nil nil) ; mtext number [number]
    ;; event
    (makeev event (default . PCE-exec-makeev) nil mtext number number char mtext)
    ;; noret
    (commit nil (default . PCE-exec-commit) nil mtext)
    (unroll nil (default . PCE-exec-unroll) nil event)
    (propdel nil (default . PCE-exec-propdel) nil prop number)
    (addmprop nil (default . PCE-exec-addmprop) nil mtext prop number number)
    (delmprop nil (default . PCE-exec-delmprop) nil prop)
    (setmprop nil (default . PCE-exec-setmprop) nil mtext prop number number)
    ;; generic
    (set generic PCE-gen-set ((v . nmtoken)) expression)
    (e generic PCE-gen-e ((f . nmtoken)) expressions)
    (tblref generic PCE-gen-tblref ((t . nmtoken)) expression)
    (propval generic PCE-gen-propval ((p . nmtoken)) prop number)))

(let ((slots PCE-elements-alist)
      elem)
  (while (setq elem (car slots))
    (put (car elem) 'PCE-element (cdr elem))
    (setq slots (cdr slots))))

(defconst PCE-document-template
  '(ordered
    (PCE single nil nil
	 (loose
	  (defvar multiple ((name . nmtoken)
			    (type . ("bool" "number" "char" "mtext" "event" "prop"))
			    (val . ("const" "init" "nil")))
	    PCE-parse-defvar any)
	  (defun multiple ((name . nmtoken)
			   (type . ("bool" "number" "char" "mtext" "event" "prop"))
			   (args . nmtokens))
	    PCE-parse-defun any)
	  (deftable multiple ((name . nmtoken)
			      (from . ("number" "char" "mtext"))
			      (to . ("bool" "number" "char" "mtext")))
	    PCE-parse-deftable any)
	  (defkeymap multiple ((name . nmtoken)
			       (to . ("bool" "number" "char" "mtext")))
	    PCE-parse-defkeymap
	    (ordered
	     (key morethan1 ((code . cdata)
			     (char . cdata)
			     (mod . cdata)))))
	  (defpattern multiple ((name . nmtoken))
	    PCE-parse-defpattern
	    (ordered
	     (menemonic morethan1 ((c . nmtoken)) PCE-parse-mnemonic any)
	     (pattern morethan1 ((e . cdata) (v . cdata)) PCE-parse-pattern)))
	  (main single nil PCE-parse-main any)))))

(defvar PCE-local-arguments nil)

(defun PCE-set-variable-constant (sym)
  (put sym 'PCE-constantp t))

(defsubst PCE-variable-constant-p (sym)
  (get sym 'PCE-constantp))

(defun PCE-check-type (type chsym)
  (cond ((eq chsym 'statement)
	 t)
	((eq chsym 'expression)
	 (memq type '(number char event mtext prop nil)))
	((eq chsym type)
	 t)
	(t
	 (error "Type mismatch:%S, %S." type chsym))))

(defun PCE-check-literal-type (l chsym)
  (cond ((or (eq chsym 'statement)
	     (eq chsym 'expression))
	 t)
	((numberp l)
	 (PCE-check-type 'number chsym)
	 l)
	((stringp l)
	 (PCE-check-type 'mtext chsym)
	 l)
	((eq (car l) 'char)
	 (PCE-check-type 'char chsym)
	 l)
	(t
	 (error "Type mismatch:%S, %S." l chsym))))

;; move to the next token.  If the token is the required element,
;; return the point of the end of the start tag.  If fail to
;; find the next token, return 'end.  Otherwise, return nil.
;; Move the current point to the head of the next token.
;;
;; Note that:
;; (match-string 1) must be element name.
;; (match-string 2) must be attr. string.
(defun PCE-next-element (req)
  (if (not (re-search-forward (concat "[^" EIMIL-space "]") nil t))
      'end
    (backward-char)
    (let ((pt (point)))
      (prog1
	  (if (and (re-search-forward (EIMIL-construct-stag-regexp 'any))
		   (= pt (match-beginning 0))
		   (string= (match-string 1) (symbol-name req)))
	      (match-end 0))
	(goto-char pt)))))

;; PCE token
;; NUMBER, (char . number or char), mtext, SYMBOL==>varialbe ref.
(defun PCE-convert-token (eobj)
  (let (st end str sym)
    (if (not (re-search-forward (concat "[^" EIMIL-space "]") nil t))
	;; end
	'end
      (backward-char)
      (cond ((eq (following-char) ?<)
	     ;; element
	     t)
	    ((eq (following-char) ?')
	     ;; char
	     (forward-char)
	     (setq st (point)
		   end (search-forward "'" nil t))
	     (if (not end)
		 (error "Unmatched \"'\"."))
	     (setq str (EIMIL-resolve-reference (buffer-substring st (1- end))))
	     (if (/= (length str) 1)
		 (error "Invalid character literal:%s" str))
	     (cons 'char (string-to-char str)))
	    ((eq (following-char) ?\")
	     ;; mtext
	     (forward-char)
	     (setq st (point)
		   end (search-forward "\"" nil t))
	     (if (not end)
		 (error "Unmatched '\"'."))
	     (EIMIL-resolve-reference (buffer-substring st (1- end))))
	    ((string-match "[0-9]" (char-to-string (following-char)))
	     ;; number
	     (setq st (point)
		   end (re-search-forward EIMIL-space-regexp nil t))
	     (if end 
		 (setq end (1- end))
	       (setq end (point-max)))
	     (setq str (buffer-substring st end))
	     (goto-char end)
	     (cond ((string-match "\\`\\([0-9]\\|[1-9][0-9]+\\)\\'" str)
		    (string-to-number (match-string 0 str)))
		   ((string-match "\\`0x\\([0-9]+\\)\\'" str)
		    (string-to-number (match-string 1 str) 16))
		   (t
		    (error "Invalid number literal:%s" str))))
	    (t
	     ;; symbol ==> variable ref
	     (if (not (re-search-forward "[A-z][A-z0-9]*" nil t))
		 (error "Invalid token."))
	     (setq sym (intern (match-string 0)))
	     (if (assq sym PCE-local-arguments)
		 sym
	       (setq sym (EIMIL-intern-soft eobj (match-string 0)))
	       (if (not sym)
		   (error "%s is not defined." (match-string 0))
		 ;; Should we unintern the symbol for the efficiency?
		 (if (not (eq (EIMIL-symbol-category sym)
			      'variable))
		     (error "%S is not a variable." sym))
		 sym)))))))

(defun PCE-parse-statement-element (eobj &optional mainp)
  (if (not (EIMIL-search-stag 'any))
      nil
    (let ((pt (match-end 0))
	  sym attrs edata sub sube func type pt-end arg args result)
      (setq sym (intern (match-string 1))
	    edata (get sym 'PCE-element))
      (if (null edata)
	  (error "Invalid element:%S" sym))
      (setq attrs (EIMIL-parse-attrs
		   (match-string 2)
		   (nth 2 edata))
	    sub (nthcdr 3 edata)
	    func (nth 1 edata)
	    type (car edata))
      (if sub
	  (if (EIMIL-search-etag
	       (list sym 'multiple (nth 2 edata) nil (if sub 'any)))
	      (setq pt-end (match-end 0))
	    (error "Cannot find the expected end tag."))
	(setq pt-end pt))
      (save-excursion
	(save-restriction
	  (narrow-to-region pt (point))
	  (goto-char (point-min))
	  (while sub
	    (setq sube (car sub))
	    (cond ((eq sube 'statements)
		   (while (not (eq 'end (setq arg (PCE-parse-statement
						   eobj 'statement mainp args))))
		     (setq args (cons arg args))))
		  ((eq sube 'expressions)
		   (while (not (eq 'end (setq arg (PCE-parse-statement
						   eobj 'expression mainp args))))
		     (setq args (cons arg args))))
		  ((eq sube 'numbers)
		   (while (not (eq 'end (setq arg (PCE-parse-statement
						   eobj 'number mainp args))))
		     (setq args (cons arg args))))
		  ((null sube)
		   ;; Do nothing.
		   ;; Assign parsing works to a generation function.
		   nil)
		  (t
		   (setq args (cons (PCE-parse-statement eobj sube mainp)
				    args))))
	    (setq sub (cdr sub)))
	  (setq result
		(cond ((and (functionp func)
			    (null sube))
		       (funcall func eobj attrs mainp))
		      ((and (consp func)
			    (eq 'default (car func)))
		       (PCE-gen-default eobj sym (cdr func) (nreverse args)))
		      ((functionp func)
		       (funcall func eobj attrs (nreverse args) mainp))
		      (t nil)))))
      (goto-char pt-end)
      result)))

(defun PCE-parse-statement (eobj type mainp &optional optionalp)
  (let ((tok (PCE-convert-token eobj))
	r)
    (cond ((eq tok t)
	   (setq r (PCE-parse-statement-element eobj mainp))
	   (PCE-check-type (car r) type)
	   (cdr r))
	  ((numberp tok)
	   (PCE-check-type 'number type)
	   tok)
	  ((stringp tok)
	   (PCE-check-type 'mtext type)
	   tok)
	  ((and (eq tok 'end)
		(or optionalp
		    (eq type 'statement)))
	   'end)
	  ((symbolp tok)
	   (PCE-check-type
	    (or (cdr (assq tok PCE-local-arguments))
		(EIMIL-symbol-type tok))
	    type)
	   tok)
	  ((eq (car tok) 'char)
	   (PCE-check-type 'char type)
	   (cdr tok))
	  (t
	   (error "Invalid argument.")))))

(defun PCE-parse-defvar (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "defvar element must have `name' attribute.")))
	       'variable nil))
	(type (intern
	       (nth 1 (or (assq 'type attrs)
			  (error "defvar element must have `type' attribute.")))))
	(val (nth 1 (assq 'val attrs)))
	(init (PCE-convert-token eobj)))
    (if val
	(progn
	  (setq val (intern val))
	  (PCE-check-literal-type init type))
      (if (eq init 'end)
	  (setq init nil)
	(error "Variable %s could not have an initial value." init )))
    (EIMIL-symbol-set-type name type)
    (if (eq val 'const)
	(PCE-set-variable-constant name))
    (set name init)
    (if (not (eq 'end (PCE-convert-token eobj)))
	(error "Invalid token."))))

(defun PCE-parse-defun (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "defun element must have `name' attribute.")))
	       'function nil))
	(type (intern
	       (nth 1 (or (assq 'type attrs)
			  (error "defun element must have `type' attribute.")))))
	(args (cdr (or (assq 'args attrs)
		       (error "defun element must have `args' attribute."))))
	PCE-local-arguments args2 argstr arglist type sym exp result)
    (setq args2 args)
    (while (setq argstr (car args))
      (if (not (string-match PCE-arglist-regexp argstr))
	  (error "Invalid argument:%s" argstr))
      (setq sym (intern (match-string 1 argstr))
	    type (intern (match-string 2 argstr))
	    arglist (cons sym arglist)
	    args (cdr args))
      (if (memq sym PCE-local-arguments)
	  (error "Invalid argument list:%S" args2))
      (setq PCE-local-arguments
	    (cons (cons sym type) PCE-local-arguments)))
    (while (not (eq 'end (setq exp (PCE-parse-statement eobj 'statement nil))))
      (setq result (cons exp result)))
    (EIMIL-symbol-set-type name type)
    (set name 
	 (byte-compile (nconc (list 'lambda (nreverse arglist)) (nreverse result))))))

(defsubst PCE-get-tblkeymaxsize (sym)
  (or (car (get sym 'PCE-table-key-val-size))
      (error "Key type of table:%S is not mtext." sym)))

(defsubst PCE-get-tblvalmaxsize (sym)
  (or (cdr (get sym 'PCE-table-key-val-size))
      (error "Value type of table:%S is not mtext." sym)))

(defun PCE-parse-deftable (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "deftable element must have `name' attribute.")))
	       'table nil))
	(from (intern
	       (nth 1 (or (assq 'from attrs)
			  (error "deftable element must have `from' attribute.")))))
	(to (intern
	     (nth 1 (or (assq 'to attrs)
			(error "defvar element must have `to' attribute.")))))
	(hash (PCE-make-hash-table))
	(keymaxsize 0)
	(valmaxsize 0)
	key val)
    (while (not (eq 'end (setq key (PCE-convert-token eobj))))
      (if (eq 'end (setq val (PCE-convert-token eobj)))
	  (error "deftable does not have the corresponding value to %S." key))
      (PCE-check-literal-type key from)
      (PCE-check-literal-type val to)
      (PCE-put-hash hash key val)
      (if (eq from 'mtext) (setq keymaxsize (max (length key) keymaxsize)))
      (if (eq to 'mtext) (setq valmaxsize (max (length key) valmaxsize))))
    (EIMIL-symbol-set-type name (cons from to))
    (set name hash)
    (put name 'PCE-table-key-val-size
	 (cons (if (eq from 'mtext) keymaxsize)
	       (if (eq to 'mtext) valmaxsize)))))

(defun PCE-key-attrs-to-hashkey (attrs)
  (format "keyevent:%.4x,%c,%.4x"
	  (string-to-number
	   (nth 1 (or (assq attrs 'code)
		      (error "key element must have code attribute."))))
	  (string-to-char
	   (nth 1 (or (assq attrs 'char)
		      (error "key element must have char attribute."))))
	  (string-to-number
	   (nth 1(or (assq attrs 'mod)
		     (error "key element must have mod attribute."))))))

(defun PCE-event-to-hashkey (ev)
  (format "%s:%.4x,%c,%.4x"
	  (aref ev 0) (aref ev 1) (aref ev 2) (aref ev 3)))

(defun PCE-parse-defkeymap (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "defkeymap element must have `name' attribute.")))
	       'keymap nil))
	(to (intern
	     (nth 1 (or (assq 'to attrs)
			(error "defkeymap element must have `to' attribute.")))))
	(hash (PCE-make-hash-table)))
    (EIMIL-parse-element
     eobj
     '(ordered
       (key
	morethan1
	((code . cdata) (char . cdata) (mod . cdata))
	(lambda (eobj attrs)
	  (let (tok)
	    (setq tok (PCE-convert-token eobj))
	    (PCE-check-literal-type tok to)
	    (PCE-put-hash
	     hash (PCE-key-attrs-to-hashkey attrs)
	     tok)
	    (if (not (eq 'end (PCE-convert-token eobj)))
		(error "Invalid token.")))))))
    (EIMIL-symbol-set-type name to)
    (set name hash)))

(defun PCE-pattern-to-regexp (pat)
  (let ((i 0)
	(regexp "")
	(len (length pat))
	(d 0)
	j c)
    (if (not (string-match "\\`[A-z().^$]+\\'" pat))
	(error "Invalid char in pattern. %s" pat))
    (while (< i len)
      (if (string-match "[()]" pat i)
	  (progn
	    (setq j (match-beginning 0)
		  c (aref pat j))
	    (cond ((eq c ?\))
		   (if (/= d 1)
		       (error "Invalid paren usage in %s." pat))
		   (setq regexp (concat regexp (substring pat i j)
					"\\)")
			 d (1- d)))
		  ((eq c ?\()
		   (if (/= d 0)
		       (error "Invalid paren usage in %s." pat))
		   (setq regexp (concat regexp (substring pat i j)
					"\\(")
			 d (1+ d))))
	    (setq i (match-end 0)))
	(setq regexp (concat regexp (substring pat i))
	      i len)))
    (if (/= d 0)
	(error "There exists an unmatched paren in %s." pat))
    regexp))

;; TODO!!! we should check if the index number is valid
;; in terms of the given pattern expression.
(defun PCE-pattern-result-to-sexp (pat)
  (let ((i 0)
	(sexp '(""))
	(len (length pat))
	j k c idx)
    (while (< i len)
      (if (string-match "\\\\" pat i)
	  (progn
	    (setq j (match-beginning 0)
		  k (1+ j))
	    (if (>= k len)
		(error "Invalid backslash usage in %s." pat))
	    (setq c (aref pat k))
	    (cond ((eq c ?\\)
		   (if (stringp (car sexp))
		       (setq sexp (cons (concat (car sexp) (substring pat i j) "\\")
					(cdr sexp)))
		     (setq sexp (cons "\\" sexp))))
		  ((memq c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		   (if (> j i)
		       (if (stringp (car sexp))
			   (setq sexp (cons (concat (car sexp) (substring pat i j))
					    (cdr sexp)))
			 (setq sexp (cons (substring pat i j) sexp))))
		   (setq idx (string-to-number (char-to-string c)))
		   (setq sexp (cons (list 'substring
					  'ostr
					  (list 'match-beginning idx)
					  (list 'match-end idx))
				    sexp)))
		  (t
		   (error "Invalid backslash usage in %s." pat)))
	    (setq i (1+ k)))
	(if (stringp (car sexp))
	    (setq sexp (cons (concat (car sexp) (substring pat i)) (cdr sexp)))
	  (setq sexp (cons (substring pat i) sexp)))
	(setq i len)))
    (if (and (= (length sexp) 1) (stringp (car sexp)))
	(car sexp)
      (list 'lambda '(ostr)
	    (cons 'concat (nreverse sexp))))))

(defun PCE-parse-defpattern (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "defpattern element must have `name' attribute.")))
	       'pattern nil))
	mns ps)
    (EIMIL-parse-element
     eobj
     '(ordered
       (mnemonic
	morethan1
	((c . cdata))
	(lambda (eobj attrs)
	  (let ((mn  (cdr (or (assq 'c attrs)
			       (error "mnemonic element must have c attribute."))))
		(str  "")
		tok)
	    (if (not (string-match "\\`[A-z]\\'" mn))
		(error "Invalid mnemonic %s." mn))
	    (setq mn (string-to-char mn))
	    (while (not (eq 'end (setq tok (PCE-convert-token eobj))))
	      (PCE-check-literal-type tok 'char)
	      (setq str (concat str (cdr tok))))
	    (setq mns (cons (cons mn str) mns))))
	any)
       (pattern
	morethan1
	((e . cdata) (v . cdata))
	(lambda (eobj attrs)
	  (setq ps (cons (cons (PCE-pattern-to-regexp e)
			       (PCE-pattern-result-to-sexp v))))
	  ))))
    (set name (cons (nreverse mns) (nreverse ps)))))

(defun PCE-parse-main (eobj attrs)
  (let ((PCE-label-counter 1)
	sttable PCE-local-arguments exp result)
    (while (not (eq 'end (setq exp (PCE-parse-statement eobj 'statement t))))
      (setq result (cons exp result)))
    (if (PCE-state-list eobj)
	(setq sttable
	      (append '(cond ((null state)))
		      (mapcar
		       (lambda (x)
			 (list
			  (list 'eq 'state (list 'quote x))
			  (list 'PCE-exec-go x)))
		       (PCE-state-list eobj)))))
    (PCE-set-main-function
     eobj
     (byte-compile
      (append
       `(lambda (state))
       (list sttable)
       (nreverse result)))
     )))

(defun PCE-parse-element (eobj sym)
  (PCE-init-eobj eobj)
  (EIMIL-parse-element eobj PCE-document-template))

;;;
;;; code generation part.
;;;

(defvar PCE-label-counter nil)

(defun PCE-generate-label (eobj)
  (let* ((n PCE-label-counter)
	 (sym (EIMIL-intern-symbol
	       eobj (format ":label-%d" n)
	       'label nil)))
    (set sym nil)
    (setq PCE-label-counter (1+ n))
    sym))

(defun PCE-gen-default (eobj sym func args)
  (let ((type (car (get sym 'PCE-element))))
    (cons type (cons func args))))

(defun PCE-gen-go (eobj attrs mainp)
  (if (not mainp)
      (error "`go' element is not allowed out of `main' element."))
  (let ((sym (EIMIL-intern-symbol
	      eobj
	      (nth 1 (or (assq 'to attrs)
			 (error "`go' element must have `to' attribute.")))
	      'label nil)))
    (set sym nil)
    (cons 'fc (list 'PCE-exec-go sym))))

(defun PCE-gen-label (eobj attrs mainp)
  (if (not mainp)
      (error "`label' element is not allowed out of `main' element."))
  (let ((sym (EIMIL-intern-symbol
	      eobj
	      (nth 1 (or (assq 'name attrs)
			 (error "`label' element must have `name' attribute.")))
	      'label nil)))
    (set sym nil)
    (cons 'fc (list 'PCE-exec-label sym))))

(defun PCE-gen-next (eobj attrs args mainp)
  (let ((label (PCE-generate-label eobj))
	(remove (nth 1 (assq 'remove attrs))))
    (PCE-add-state eobj label)
    (setq remove (string= remove "true"))
    (cons 'fc
	  (list 'progn
		(list 'PCE-exec-next label remove)
		(list 'PCE-exec-label label)))))

(defun PCE-gen-forward (eobj attrs args mainp)
  (let ((to
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'to attrs)
		     (error "`forward' element must have `to' attribute.")))
	  'engine nil)))
    (cons 'fc (list 'PCE-exec-send to
		    (EIMIL-intern-symbol
		     eobj "curev" 'variable nil)))))

(defun PCE-gen-send (eobj attrs args mainp)
  (let ((to
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'to attrs)
		     (error "`send' element must have `to' attribute.")))
	  'engine nil)))
    (cons 'fc (list 'PCE-exec-send to
		    (car args)))))

(defun PCE-gen-mark-undo (eobj attrs args mainp)
  (let ((label (PCE-generate-label eobj)))
    (cons 'undoc
	  (list 'progn
		(list 'PCE-exec-mark-undo label)
		(list 'PCE-exec-label label)))))

(defun PCE-gen-throw (eobj attrs args mainp)
  (cons 'fc
	(list 'signal
	      (list
	       'quote
	       (EIMIL-check-symbol
		eobj
		(nth 1 (or (assq 'exc attrs)
			   (error "`throw' element must have `exc' attribute.")))
		'exception nil)))))

(defun PCE-gen-while (eobj attrs args mainp)
  (cons 'flow
	(cons 'while args)))

(defun PCE-gen-tblkeymaxsize (eobj attrs mainp)
  (let ((tbl
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 't attrs)
		     (error "`tblkeymaxsize' element must have `t' attribute.")))
	  'table nil)))
    (cons 'number (PCE-get-tblkeymaxsize tbl))))

(defun PCE-gen-tblvalmaxsize (eobj attrs mainp)
  (let ((tbl
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 't attrs)
		     (error "`tblvalmaxsize' element must have `t' attribute.")))
	  'table nil)))
    (cons 'number (PCE-get-tblvalmaxsize tbl))))

(defun PCE-gen-makeprop (eobj attrs mainp)
  (let ((p
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'p attrs)
		     (error "`makeprop' element must have `p' attribute.")))
	  'property nil)))
    (cons 'prop (list 'quote (cons p (vector nil nil nil nil))))))

(defun PCE-gen-getmprop (eobj attrs args mainp)
  (let ((p
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'p attrs)
		     (error "`getmprop' element must have `p' attribute.")))
	  'property nil)))
    (cons 'prop (list 'PCE-exec-getmprop p (car args) (nth 1 args)))))

(defun PCE-gen-findmprop (eobj attrs args mainp)
  (let ((p
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'p attrs)
		     (error "`findmprop' element must have `p' attribute.")))
	  'property nil)))
    (cons 'prop (list 'PCE-exec-findmprop p (car args) (nth 1 args)))))

(defun PCE-gen-interact (eobj attrs mainp)
  (let ((op
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'op attrs)
		     (error "`interact' element must have `op' attribute.")))
	  'operation nil)))
    (cons 'mtext (list 'PCE-exec-interact op))))

(defun PCE-gen-match (eobj attrs args mainp)
  (if (/= (length args) 1)
      (error "too many arguments in `match' element."))
  (let ((p
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'p attrs)
		     (error "`match' element must have `p' attribute.")))
	  'pattern nil)))
    (cons 'mtext (list 'PCE-exec-match p (car args)))))

(defun PCE-gen-set (eobj attrs args mainp)
  (if (/= (length args) 1)
      (error "too many arguments in `set' element."))
  (let ((v
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'v attrs)
		     (error "`set' element must have `v' attribute.")))
	  'variable nil)))
    (cons (EIMIL-symbol-type v) (list 'PCE-exec-set
				      (list 'quote v)
				      (car args)))))

(defun PCE-gen-e (eobj attrs args mainp)
  (let ((f
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 'f attrs)
		     (error "`e' element must have `f' attribute.")))
	  'function nil)))
    ;; TODO!!! we should check arguemnt type and number.
    (cons (EIMIL-symbol-type f) (nconc (list 'PCE-exec-e f) args))))

(defun PCE-gen-tblref (eobj attrs args mainp)
  (let ((tbl
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 't attrs)
		     (error "`tblref' element must have `t' attribute.")))
	  'table nil)))
    ;; TODO!!! we should check arguemnt type.
    (cons (cdr (EIMIL-symbol-type tbl)) (list 'PCE-exec-tblref tbl (car args)))))

(defun PCE-gen-propval (eobj attrs args mainp)
  (let ((p
	 (EIMIL-check-symbol
	  eobj
	  (nth 1 (or (assq 't attrs)
		     (error "`propval' element must have `p' attribute.")))
	  'property nil)))
    (cons (EIMIL-symbol-type p)
	  (list 'PCE-exec-propval
		p (car args) (nth 1 args)))))

;; <try>
;;  statements
;; <catch> statements </catch>
;; <catch> statements </catch>
;;  ...
;; </try>
(defun PCE-gen-try (eobj attrs mainp)
  (let (try-clause catch-clauses clause name elem st end)
    ;; try
    (while (not (setq st (PCE-next-element 'catch)))
      (setq try-clause (cons (PCE-parse-statement eobj 'statement mainp) try-clause)))
    ;; catch
    (while (not (eq st 'end))
      (save-restriction
	(setq attrs (EIMIL-parse-attrs (match-string 2) '((exc . nmtoken)))
	      name (nth 1 (or (assq 'exc attrs)
			      (error "catch element must have `name' attribute."))))
	(setq name (EIMIL-check-symbol eobj name 'exception t))
	(EIMIL-search-etag '(catch nil nil nil 'any))
	(setq end (match-end 0))
	(narrow-to-region st (point))
	(goto-char st)
	(while (not (eq 'end (setq elem (PCE-parse-statement eobj 'statement mainp))))
	  (setq clause (cons elem clause))))
      (setq catch-clauses (cons (cons name (nreverse clause)) catch-clauses))
      (goto-char end)
      (setq st (PCE-next-element 'catch)))
    (if (not (eq 'end (PCE-convert-token eobj)))
	(error "Don't put any entities after `catch' element."))
    (cons 'statement (append (list 'condition-case 'var
				   (cons 'progn (nreverse try-clause)))
			     (nreverse catch-clauses)))))

;; <if> statements <else> statement </else> </if>
(defun PCE-gen-if (eobj attrs mainp)
  (let ((exp (PCE-parse-statement eobj 'expression mainp))
	tclause eclause elem st end)
    (while (not (setq st (PCE-next-element 'else)))
      (setq tclause (cons (PCE-parse-statement eobj 'statement mainp) tclause)))
    (if (not (eq st 'end))
	(progn
	  (save-restriction
	    (goto-char st)
	    (EIMIL-search-etag '(else nil nil nil 'any))
	    (setq end (match-end 0))
	    (narrow-to-region st (point))
	    (goto-char st)
	    (while (not (eq 'end (setq elem (PCE-parse-statement
					     eobj 'statement mainp))))
	      (setq eclause (cons elem eclause))))
	  (goto-char end)
	  (if (not (eq 'end (PCE-convert-token eobj)))
	      (error "Don't put any entities after `else' element."))))
    (cons 'statement (append (list 'if exp (cons 'progn (nreverse tclause)))
			     (nreverse eclause)))))

(defun PCE-keycase-attrs-to-list (attrs)
  (let ((code (nth 1 (assq 'code attrs)))
	(char (nth 1 (assq 'char attrs)))
	(mod (nth 1 (assq 'mod attrs))))
    (if (or code char mod)
	(list (if code (string-to-number code))
	      (if char (string-to-char char))
	      (if mod (string-to-char mod)))
      (error "`keycase' element must have at least `code', `char', or `mod' attribute."))))

;; <select>
;;  <keycase> statements </keycase>
;;  <keymap> statements </keymap>
;;  ...
;;  <default> statements </default>
;; </select>
(defun PCE-gen-select (eobj attrs mainp)
  (let (clauses clause type espec exp elem st end)
    (while
	(progn
	  (cond ((setq st (PCE-next-element 'keycase))
		 (setq type 'keycase
		       espec '(keycase
			       multiple
			       ((code . cdata)
				(char . cdata)
				(mod . cdata))
			       nil any))
		 (setq attrs (EIMIL-parse-attrs
			      (match-string 2)
			      (nth 2 espec)))
		 (setq exp (cons
			    'PCE-exec-keycase
			    (PCE-keycase-attrs-to-list attrs))))
		((setq st (PCE-next-element 'keymap))
		 (let ((keymapsym
			(EIMIL-check-symbol
			 eobj 
			 (nth 1 (or (assq 'k attrs)
				    (error "keymap element must have k attribute.")))
			 'keymap nil)))
		   (setq type 'keymap
			 espec '(keymap
				 multiple
				 ((k . nmtoken))
				 nil any))
		   (setq attrs (EIMIL-parse-attrs
				(match-string 2)
				(nth 2 espec)))
		   (setq exp (list 'PCE-exec-keymap keymapsym))
		   ;; set the type of `mapval' variable, which is dynamically
		   ;; changed by `keymap'.
		   (EIMIL-symbol-set-type
		    (EIMIL-intern-soft eobj "mapval")
		    (EIMIL-symbol-type keymapsym))))
		((setq st (PCE-next-element 'default))
		 (setq type 'default
		       espec '(default single nil nil any))
		 (setq exp t))
		(t
		 (error "Invalid element in select.")))
	  (if (eq st 'end)
	      nil
	    (save-restriction
	      (goto-char st)
	      (EIMIL-search-etag espec)
	      (setq end (match-end 0))
	      (narrow-to-region st (point))
	      (goto-char st)
	      (while (not (eq 'end (setq elem (PCE-parse-statement
					       eobj 'statement mainp))))
		(setq clause (cons elem clause))))
	    (goto-char end)
	    (setq clauses (cons (cons exp (nreverse clause)) clauses)
		  clause nil)
	    (if (eq type 'default)
		(if (not (eq 'end (PCE-convert-token eobj)))
		    (error "Don't put any entities after `default' element.")
		  nil)
	      t))))
    (cons 'statement (cons 'cond (nreverse clauses)))))

(defun PCE-gen-return (eobj attrs args mainp)
  (if mainp
      (error "`return' element is not allowed in `main' element."))
  (cons 'flow
	(cons 'throw (cons '(quote PCE-return) args))))

;; <concat> char or mtext .. </concat>
(defun PCE-gen-concat (eobj attrs mainp)
  (let (tok args type)
    (while (not (eq 'end (setq tok (PCE-convert-token eobj))))
      (if (eq tok t)
	  (setq tok (PCE-parse-statement-element eobj mainp)))
      (cond ((or (stringp tok)
		 (and (listp tok)
		      (eq (car tok) 'mtext)))
	     (setq args (cons (cdr tok) args)))
	    ((and (listp tok)
		  (eq (car tok) 'char))
	     (if (consp (cdr tok))
		 (setq args (cons (list 'char-to-string (cdr tok)) args))
	       (setq args (cons (char-to-string (cdr tok)) args))))
	    ((and (symbolp tok)
		  (setq type (or (cdr (assq tok PCE-local-arguments))
				 (EIMIL-symbol-type tok)))
		  (memq type '(mtext char)))
	     (if (eq 'mtext type)
		 (setq args (cons tok args))
	       (setq args (cons (list 'char-to-string tok) args))))
	    (t
	     (error "Arguments of `concat' element must be char or mtext."))))
    (cons 'mtext (cons 'PCE-exec-concat (nreverse args)))))

;; <concat> mtext BEGIN [END] </concat>
(defun PCE-gen-substr (eobj attrs mainp)
  (let ((mtext (PCE-parse-statement eobj 'mtext mainp))
	(beg (PCE-parse-statement eobj 'number mainp))
	(end (PCE-parse-statement eobj 'number mainp t)))
    (cons 'mtext
	  (list 'PCE-exec-substr
		mtext beg
		(if (eq 'end end) nil end)
		))))

;;;
;;; PCE event manager
;;;

(defun PCE-get-event (eobj)
  (let ((slot (aref (EIMIL-private eobj) 1))
	ev)
    (if slot
	(progn
	  (aset (EIMIL-private eobj) 1 (cdr slot))
	  (car slot))
      nil)))

(defun PCE-add-unread-event (eobj event)
  (aset (EIMIL-private eobj) 1
	(nconc (aref (EIMIL-private eobj) 1) (list event))))

;;;
;;; IIIMP based communication.
;;;

;;;
;;; PCE runtime function part.
;;;

(defvar PCE-current-eobj nil)
(defvar PCE-current-event nil)

(defun PCE-forward (eobj event)
  (let ((PCE-current-eobj eobj)
	(PCE-current-event event))
    (set (EIMIL-intern-symbol
	  eobj "curev" 'variable nil)
	 event)
    (PCE-set-current-state
     eobj
     (catch 'PCE-suspend
       (funcall
	(PCE-get-main-function eobj)
	(PCE-get-current-state eobj))
       nil))))

(defun PCE-exec-check-type (type obj)
  ;; TODO
  )

(defun PCE-exec-keycase (code char mod)
  (and (string= (aref PCE-current-event 0)
		"keyevent")
       (or (not code) (= code (aref PCE-current-event 1)))
       (or (not char) (eq char (aref PCE-current-event 2)))
       (or (not mod) (= mod (aref PCE-current-event 3)))))

(defun PCE-exec-keymap (keymap)
  (let ((val
	 (PCE-get-hash (symbol-value keymap)
		       (PCE-event-to-hashkey PCE-current-event))))
    (if val
	(progn
	  (set (EIMIL-intern-soft PCE-current-eobj "mapval") val)
	  t)
      nil)))

(defun PCE-exec-go ()
  ;; do nothing.
  ;; Assign the work to byte-compiler code generator.
  )
(defun PCE-exec-label ()
  ;; do nothing
  ;; Assign the work to byte-compiler code generator.
  )

(defun PCE-exec-interact (op)
  ;;; TODO
)

(defun PCE-exec-match (p mtext)
  (let* ((len (length mtext))
	 (val (symbol-value p))
	 (ns (car val))
	 (ps (cdr val))
	 (i 0)
	 (rstr "")
	 ns2 pse c)
    (catch 'fin
      (while (< i len)
	(setq c (aref mtext i)
	      ns2 ns)
	(while (not
		(if (memq c (cdr ns2))
		    (setq rstr (concat rstr (char-to-string (car ns))))))
	  (setq ns2 (cdr ns2))
	  (if (null ns2) (throw 'fin nil))))
      (while (setq pse (car ps))
	(if (string-match (car pse) rstr)
	    (if (stringp (cdr pse))
		(throw 'fin (cdr pse))
	      (throw 'fin (funcall (cdr pse) mtext))))
	(setq ps (cdr ps)))
      nil)))

(defun PCE-exec-set (v arg)
  (set v arg))

(defun PCE-exec-e (f &rest args)
  ;; TODO: We should check the result.
  (catch 'PCE-return
    (apply f args)))

(defun PCE-exec-tblref (tbl obj)
  (PCE-get-hash tbl obj))

(defun PCE-exec-propval (p prop idx)
  (if (not (eq (car prop) p))
      (error "Invalid property:%S, %S" (car prop) p))
  (aref (cdr prop) idx))

(defun PCE-exec-mark-undo (label))
(defun PCE-exec-undo ())

(defun PCE-exec-send (to ev)
  (PCE-forward (nth 2 (symbol-value to)) ev))

(defun PCE-exec-next (label removep)
  (PCE-exec-set
   (EIMIL-intern-symbol
    PCE-current-eobj "curev"
    'variable nil)
   (or (PCE-get-event PCE-current-eobj)
       (throw 'PCE-suspend label))))

(defun PCE-exec-toggle-preedit (flag)
  (EIMIL-toggle-preedit-text PCE-current-eobj flag))

(defun PCE-exec-toggle-lookup-choice (flag)
  (EIMIL-toggle-lookup-choice PCE-current-eobj flag))

(defun PCE-exec-keyeventp (event)
  (string= "keyevent" (aref 0 event)))

(defun PCE-exec-propsize (prop)
  (length (aref (cdr prop) 3)))

(defun PCE-exec-propmbeg (prop)
  (aref (cdr prop) 1))

(defun PCE-exec-propmend (prop)
  (aref (cdr prop) 2))

(defun PCE-exec-evval (event)
  (aref event 1))

(defun PCE-exec-evmod (event)
  (aref event 3))

(defun PCE-exec-strcmp (m1 m2)
  (let ((i 0)
	(len (min (length m1) (length m2))))
    (while (and (< i len)
		(= (char-to-ucs (aref m1 i))
		   (char-to-ucs (aref m2 i))))
      (setq i (1+ i)))
    (if (= i len)
	(if (> (length m1) (length m2)) 1
	  (if (< (length m1) (length m2)) -1
	    0))
      (if (> (char-to-ucs (aref m1 i))
	     (char-to-ucs (aref m2 i))) 1
	-1))))

;; We should combine type checking with static code generator.
(defun PCE-exec-propadd (prop obj)
  (PCE-exec-check-type (EIMIL-symbol-type (car prop)) obj)
  (aset (cdr prop) 3 (nconc (aref (cdr prop) 3) (list obj))))

(defun PCE-exec-propcopy (prop)
  (cons (car prop) (copy-sequence (cdr prop))))

(defun PCE-exec-evchar (event)
  (aref event 2))

(defun PCE-exec-makechar (n)
  (ucs-to-char n))

(defun PCE-exec-UCSval (char)
  (char-to-ucs char))

(defun PCE-exec-evtype (event)
  (aref event 0))

(defun PCE-exec-evmtext (event)
  (aref event 4))

(defun PCE-exec-makeev (type val char mod mtext)
  (vector type val char mod mtext))

(defun PCE-exec-commit (mtext)
  ;;; TODO: OP-sych...
  ;;(EIMIL-commit mtext)
  (EIMIL-commit-string PCE-current-eobj mtext))

(defun PCE-exec-unroll (event)
  (PCE-add-unread-event PCE-current-eobj event))

(defun PCE-exec-propdel (prop idx)
  (let ((slot (aref (cdr prop) 3)))
    (if (or (< idx 0)
	    (>= idx (length slot)))
	(error "propdel: idx is out of the range of prop:%d" idx))
    (aset (cdr prop) 3 (delq (nth idx slot) slot))))

;;
;; mtext and property
;;
;;  
;; Notation: |----| mtext
;;           <----> property
;;           (----) Emacs text property.
;;
;; |-------------------------------------------|
;;           <--P1-->                 <--P4-->
;;                         <--P2->
;;               <---P3----->
;;           (-------------------)    (------)
;;                P:(P3 P2 P1)         P:(P4)
;; Pn: [MTEXT BEG END VALUES]

(defun PCE-exec-getmprop (p mtext pos)
  (let ((props (get-text-property pos p mtext))
	ext)
    (while
	(and (setq ext (car props))
	     (or (< pos (aref ext 1))
		 (>= pos (aref ext 2))))
      (setq props (cdr props)))
    (if ext
	(cons p ext))))

(defun PCE-exec-findmprop (p mtext pos)
  (let ((pos (next-single-property-change pos p mtext))
	props ext)
    (if pos
	(progn
	  (setq props (get-text-property pos p mtext))
	  (while
	      (and (setq ext (car props))
		   (or (< pos (aref ext 1))
		       (>= pos (aref ext 2))))
	    (setq props (cdr props)))
	  (if ext
	      (cons p ext)))
      nil)))

(defun PCE-exec-delmprop (prop)
  (let* ((p (car prop))
	 (ext (cdr prop))
	 (mtext (aref ext 0))
	 beg mbeg mend)
    (if mtext
	(progn
	  (setq beg (aref ext 1)
		mbeg (or (previous-single-property-change beg p mtext)
			 beg)
		mend (or (next-single-property-change beg p mtext)
			 (aref ext 2)))
	  (put-text-property mbeg mend p
			     (delq ext (get-text-property beg p mtext))
			     mtext)
	  (aset ext 0 nil)
	  (aset ext 1 nil)
	  (aset ext 2 nil)
	  nil))))

(defun PCE-exec-addmprop (mtext prop beg end)
  (let* ((p (car prop))
	 (ext (cdr prop))
	 (pos (text-property-not-all beg end p nil mtext))
	 props ext2 props2)
    (if (aref ext 0) (PCE-exec-delmprop prop))
    (aset ext 0 mtext)
    (aset ext 1 beg)
    (aset ext 2 end)
    (if pos
	(progn
	  (setq props (get-text-property pos p mtext))
	  (while (and (setq pos (next-single-property-change pos p mtext end))
		      (< pos end))
	    (setq props (nconc props (get-text-property pos p mtext))))
	  (setq props2 props)
	  (while (setq ext2 (car props))
	    (if (< (aref ext2 1) beg) (setq beg (aref ext2 1)))
	    (if (> (aref ext2 2) end) (setq end (aref ext2 2)))
	    (setq props (cdr props)))
	  (put-text-property beg end p (cons ext props2) mtext))
      (put-text-property beg end p (list ext) mtext))
    nil))


(defun PCE-set-mtext-props (p mtext props)
  (let (ext beg end nprops)
    (if props
	(progn
	  (setq beg (aref (car props) 1)
		end (aref (car props) 2))
	  (while (setq ext (car props))
	    (if (> (min end (aref ext 2))
		   (max beg (aref ext 1)))
		(setq nprops (cons ext nprops)
		      beg (min beg (aref ext 1))
		      end (max end (aref ext 2)))
	      (if nprops
		  (put-text-property beg end p nprops mtext))
	      (setq nprops (list ext)
		    beg (aref ext 1)
		    end (aref ext 2)))
	    (setq props (cdr props)))
	  (if nprops
	      (put-text-property beg end p nprops mtext))))))

;; |-------------------------------------------|
;;           <--P1-->                 <--P4-->
;;                         <--P2->              
;;               <---P3----->
;;           (-------------------)    (------)
;;                P:(P3 P2 P1)         P:(P4)
;;
;;
;;                    <==setmprop:P5=====>
;;
;;                then...
;;
;; |-------------------------------------------|
;;           <--P1-->                     <P4>
;;                         XXP2XXX              
;;               <P3->
;;                    <==setmprop:P5=====>
;;           (-------)(------------------)(--)
;;           P:(P3 P1)       P:(P5)       P:P4
;;
;;
(defun PCE-exec-setmprop (mtext prop beg end)
  (let* ((p (car prop))
	 (ext (cdr prop))
	 (pos (text-property-not-all beg end p nil mtext))
	 props mbeg mend ext2 extn props-b props-a)
    (if (aref ext 0) (PCE-exec-delmprop prop))
    (aset ext 0 mtext)
    (aset ext 1 beg)
    (aset ext 2 end)
    (if pos
	(progn
	  (setq props (get-text-property pos p mtext))
	  (while (and (setq pos (next-single-property-change pos p mtext end))
		      (< pos end))
	    (setq props (nconc props (get-text-property pos p mtext))))

	  (while (setq ext2 (car props))
	    (setq mbeg (aref ext2 1)
		  mend (aref ext2 2))
	    (cond ((and (>= mend beg)
			(< mbeg beg))
		   ;; <--->
		   ;;   <===>
		   (aset ext2 2 beg)
		   (setq props-b (cons ext2 props-b)))
		  ((and (<= mend end)
			(>= mbeg beg))
		   ;;   <--->
		   ;;  <======>
		   (aset ext2 0 nil)
		   (aset ext2 1 nil)
		   (aset ext2 2 nil))
		  ((and (<= mbeg end)
			(> mend end))
		   ;;       <--->
		   ;;  <======>
		   (aset ext2 1 end)
		   (setq props-a (cons ext2 props-a)))
		  ((and (< mbeg beg)
			(> mend end))
		   ;; <-------->
		   ;;   <====>
		   (setq extn (copy-sequence ext2))
		   (aset ext2 2 beg)
		   (aset extn 1 end)
		   (setq props-b (cons ext2 props-b))
		   (setq props-a (cons extn props-a)))
		  ((< mbeg beg)
		   ;; <---->
		   ;;       <====>
		   (setq props-b (cons ext2 props-b)))
		  (t
		   ;;        <---->
		   ;; <====>
		   (setq props-a (cons ext2 props-a))))
	    (setq props (cdr props)))
	  (put-text-property beg end p (list ext) mtext)
	  (PCE-set-mtext-props p mtext props-b)
	  (PCE-set-mtext-props p mtext props-a))
      (put-text-property beg end p (list ext) mtext))
    nil))

(defun PCE-move-mprop (mtext dif last)
  (let ((pos (if (text-properties-at 0 mtext) 0
	       (next-property-change 0 mtext)))
	slots props props2 nprops next nbeg nend modified melem)
    (while pos
      (setq slots (text-properties-at pos mtext))
      (while slots
	(setq props (car (cdr slots))
	      props2 props
	      nprops nil)
	(if (setq melem (assq props modified))
	    (setcar (cdr slots) (cdr melem))
	  (progn
	    (while props
	      (setq next (copy-sequence (car props))
		    nbeg (+ (aref next 1) dif)
		    nend (+ (aref next 2) dif))
	      (aset next 0 mtext)
	      (if (< nbeg 0)
		  (aset next 1 0)
		(aset next 1 nbeg))
	      (if (> nend last)
		  (aset next 2 last)
		(aset next 2 nend))
	      (setq nprops (cons next nprops)
		    props (cdr props)))
	    (setq nprops (nreverse nprops))
	    (setq modified (cons (cons props2 nprops) modified))
	    (setcar (cdr slots) nprops)))
	(setq slots (cdr (cdr slots))))
      (setq pos (next-property-change pos mtext)))
    mtext))

(defun PCE-exec-reset-mtext (mtext)
  (let ((pos (if (text-properties-at 0 mtext) 0
	       (next-property-change 0 mtext)))
	slots props)
    (while pos
      (setq slots (text-properties-at pos mtext))
      (while slots
	(setq props (car (cdr slots)))
	(while props
	  (aset (car props) 0 mtext)
	  (setq  props (cdr props)))
	(setq slots (cdr (cdr slots))))
      (setq pos (next-property-change pos mtext)))
    mtext))


(defun PCE-exec-substr (mtext beg end)
  (if (not end)
      (setq end (length mtext)))
  (PCE-move-mprop (substring mtext beg end) (- beg) (- end beg)))

(defun PCE-exec-concat (&rest args)
  (let* ((new (car args))
	 (pos (length new))
	 npos mtext)
    (setq args (cdr args))
    (while (setq mtext (car args))
      (setq npos (+ pos (length mtext))
	    new (concat new (PCE-move-mprop (copy-sequence mtext) pos npos))
	    pos npos)
      (setq args (cdr args)))
    (PCE-exec-reset-mtext new)
    new))

;;
;; go and label byte compiler operations.
;;

(byte-defop-compiler (PCE-exec-go nil))
(byte-defop-compiler (PCE-exec-label nil))

(defun byte-compile-PCE-exec-go (label)
  (setq label (nth 1 label))
  (let ((tag
	 (or (car (symbol-value label))
	     (car (set label (cons (byte-compile-make-tag) nil))))))
    (byte-compile-goto 'byte-goto tag)
    ;; Note that go-label combination
    (setq for-effect nil)))

(defun byte-compile-PCE-exec-label (label)
  (setq label (nth 1 label))
  (let* ((tagslot (symbol-value label))
	 (tag (or (car tagslot)
		  (byte-compile-make-tag))))
    (if (cdr tagslot)
	(error "Duplicated tag:%S" label))
    (set label (cons tag t))
    (byte-compile-out-tag tag)
    (setq for-effect nil)))

;;;
;;; API
;;;

(defun PCE-event-handler (eobj emev)
  (let* ((case-fold-search nil)
	 (iev (iiimcf-translate-emacs-event emev))
	 (code (cdr (car iev)))
	 (char (cdr (nth 1 iev)))
	 (mod (cdr (nth 2 iev))))
    (condition-case var
	(PCE-forward
	 eobj (vector
	       "keyevent"
	       code char mod ""))
      (wrong-type-arugment
       )
      (PCE-unknown-error
       ))))

(defun PCE-copy-eobj (eobj)
  (EIMIL-set-private eobj (copy-sequence (EIMIL-private eobj)))
  (PCE-set-main-function 
   eobj
   (EIMIL-copy-funcdef
    (PCE-get-main-function eobj)
    (EIMIL-privobarray eobj)
    (EIMIL-pubobarray eobj)))
  eobj)

;; IIIMP

;; Journal manager

;; UI entities

;;;
;;; Register interfaces to EIMIL.
;;;

(EIMIL-register-engine
 "com.sun.iiim.pce1.s1"
 (function PCE-parse-element)
 (function PCE-event-handler)
 (function PCE-copy-eobj))

(provide 'PCE)

;;; EIMIL.el
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

;;
;; EIMIL symbols:

;; plist
;;  EIMIL-category: CATEGORY
;;  EIMIL-publicp: publicp
;;  EIMIL-type: type (it is set only if the symbol is variable, property, or function)
;; 
;; CATEGORY		VALUE
;; ccdef:               eobj
;; variable:		as is
;; operation:           ((depend-syms . affect-syms) ...)
;; function:            function definition
;; property:            nil
;; exception:           ((lang . message) (lang . message) ...)
;; engine:              eobj
;; label:               (<byte-compiler-label> . <flag>)
;; table:               hash table
;; pattern:             (((?a . "CHARS") (?b . "CHARS") ...) . (("RE" EXP) ("RE" EXP) ...))

;;; value list
;; bool:                t/nil
;; number:              as is
;; prop:                (property-sym . [ MTEXT BEG END (value1 value2 ...)])
;; mtext:               string
;; event:               [type val(code) char mod mtext]
;; char:                character (maybe number ;_;)

;;; Code:

(require 'iiimp)

(defvar EIMIL-cache-directory "~/.eimil")

(defconst EIMIL-obarray-size 71)

(defvar EIMIL-engine-alist nil)

(defsubst EIMIL-symbol-category (sym)
  (get sym 'EIMIL-category))

(defsubst EIMIL-symbol-p (sym)
  (and (symbolp sym)
       (get sym 'EIMIL-category)))

(defsubst EIMIL-symbol-publicp (sym)
  (get sym 'EIMIL-publicp))

(defsubst EIMIL-symbol-type (sym)
  (get sym 'EIMIL-type))

(defsubst EIMIL-symbol-set-type (sym type)
  (put sym 'EIMIL-type type))

;;;
;;; compiled code manipulation (XEmacs has these functions by default).
;;;

(if (not (fboundp 'compiled-function-arglist))
    (defun compiled-function-arglist (fun)
      (aref fun 0)))
(if (not (fboundp 'compiled-function-instructions))
    (defun compiled-function-instructions (fun)
      (aref fun 1)))
(if (not (fboundp 'compiled-function-constants))
    (defun compiled-function-constants (fun)
      (aref fun 2)))
(if (not (fboundp 'compiled-function-stack-depth))
    (defun compiled-function-stack-depth (fun)
      (aref fun 3)))

;; EIMIL object internal structure
;; [<name>:0 <version-list>:1
;;  <public-obarray(shared)>:2
;;  <private-obarray>:3
;;  <inheirit-ccdef-sym-list>:4
;;  <UIdata-sym>:5
;;  <commitnotify-sym-list>:6
;;  <(engine-class . engine-name) or nil>:7
;;  <engine-specific-private>:8
;;  <handler-alist>:9
;;  <event-handler>:10
;;  <client-specific-private>:11]

(defsubst EIMIL-name (eobj) (aref eobj 0))
(defsubst EIMIL-set-name (eobj name) (aset eobj 0 name))
(defsubst EIMIL-pubobarray (eobj) (aref eobj 2))
(defsubst EIMIL-privobarray (eobj) (aref eobj 3))
(defsubst EIMIL-inherit-ccdefs (eobj) (aref eobj 4))
(defsubst EIMIL-UIdata (eobj) (aref eobj 5))
(defsubst EIMIL-set-UIdata (eobj sym) (aset eobj 5 sym))
(defsubst EIMIL-commitnotify-ops (eobj) (aref eobj 6))
(defsubst EIMIL-add-commitnotify-op (eobj sym)
  (aset eobj 6 (cons sym (aref eobj 6))))
(defsubst EIMIL-remove-commitnotify-op (eobj sym)
  (aset eobj 6 (delq sym (aref eobj 6))))
(defsubst EIMIL-engine-class (eobj) (car (aref eobj 7)))
(defsubst EIMIL-engine-name (eobj) (cdr (aref eobj 7)))
(defsubst EIMIL-set-engine-class-and-name (eobj class name)
  (aset eobj 7 (cons class name)))
(defsubst EIMIL-private (eobj) (aref eobj 8))
(defsubst EIMIL-set-private (eobj obj) (aset eobj 8 obj))
(defsubst EIMIL-handler-alist (eobj) (aref eobj 9))
(defsubst EIMIL-set-handler-alist (eobj func) (aset eobj 9 func))
(defsubst EIMIL-event-handler (eobj) (aref eobj 10))
(defsubst EIMIL-set-event-handler (eobj func) (aset eobj 10 func))
(defsubst EIMIL-client-private (eobj) (aref eobj 11))
(defsubst EIMIL-set-client-private (eobj data) (aset eobj 11 data))

(defconst EIMIL-symbol-regexp "[A-z][A-z0-9]*")
(defsubst EIMIL-check-symbol-syntax (str)
  (if (string-match EIMIL-symbol-regexp str)
      nil
    (error "Invalid symbol name:%s" str)))

(defun EIMIL-register-symbol (eobj str category &optional publicp)
  (EIMIL-check-symbol-syntax str)
  (if (or (intern-soft str (EIMIL-pubobarray eobj))
	  (intern-soft str (EIMIL-privobarray eobj)))
      (error "Symbol:%s has already been registered." str)
    (if publicp
	(let ((sym (intern str (EIMIL-pubobarray eobj))))
	  (put sym 'EIMIL-category category)
	  (put sym 'EIMIL-publicp t)
	  sym)
      (let ((sym (intern str (EIMIL-privobarray eobj))))
	(put sym 'EIMIL-category category)
	sym))))

;; Check if the symbol is registered in the specified category.
;; If it's not, raise an error.
;; If publicp is non-nil, the symbol must be a public symbol.
;; This function returns the symbol of str in eobj.
(defun EIMIL-check-symbol (eobj str category publicp)
  (let ((sym (or (intern-soft str (EIMIL-pubobarray eobj))
		 (intern-soft str (EIMIL-privobarray eobj)))))
    (if (not (and sym (eq (EIMIL-symbol-category sym) category)))
	(error "Symbol:%s is not appropriately declared." str))
    (if (and publicp
	     (not (EIMIL-symbol-publicp sym)))
	(error "Symbol:%s is not a public symbol." str))
    sym))

;; First, inspect if str is already registered in eobj.
;; If so, check if the category matches with category.
;; Otherwise, register str as a new symbol of eobj.
(defun EIMIL-intern-symbol (eobj str category &optional publicp)
  (EIMIL-check-symbol-syntax str)
  (let ((sym (or (intern-soft str (EIMIL-pubobarray eobj))
		 (intern-soft str (EIMIL-privobarray eobj)))))
    (if sym
	(if (not (eq (EIMIL-symbol-category sym) category))
	    (error "Symbol %s is already registered as different catgory. %S %S"
		   sym (EIMIL-symbol-category sym) category))
      (if publicp
	  (progn
	    (setq sym (intern str (EIMIL-pubobarray eobj)))
	    (put sym 'EIMIL-publicp t))
	(setq sym (intern str (EIMIL-privobarray eobj))))
      (put sym 'EIMIL-category category))
    sym))

(defun EIMIL-reintern-symbol (sym privob pubob)
  (let (newsym)
    (if (EIMIL-symbol-publicp sym)
	(progn
	  (setq newsym (intern (symbol-name sym) pubob))
	  (put newsym 'EIMIL-publicp t))
      (setq newsym (intern (symbol-name sym) privob)))
    (put newsym 'EIMIL-category (EIMIL-symbol-category sym))
    (if (EIMIL-symbol-type sym)
	(EIMIL-symbol-set-type newsym (EIMIL-symbol-type sym)))
    newsym))

(defun EIMIL-reintern-symbols (syms privob pubob)
  (while syms
    (EIMIL-reintern-symbol (car syms) privob pubob)
    (setq syms (cdr syms))))

(defun EIMIL-intern-soft (eobj str)
  (or (intern-soft str (EIMIL-pubobarray eobj))
      (intern-soft str (EIMIL-privobarray eobj))))

(defun EIMIL-compare-revision (rv1 rv2)
  (while (and rv1 rv2
	      (= (car rv1) (car rv2)))
    (setq rv1 (cdr rv1)
	  rv2 (cdr rv2)))
  (cond ((and (null rv1) rv2) nil)
	((and rv1 (null rv2)) t)
	((and (null rv1) (null rv2)) t)
	((< (car rv1) (car rv2)) nil)
	(t t)))
	      
(defsubst EIMIL-revision (eobj) (aref eobj 1))
(defun EIMIL-set-revision (eobj revision-str)
  (if (not (string-match "\\`[0-9]+\\(\\.[0-9]+\\)\\'" revision-str))
      (error "Invalid revision string:%s" revision-str))
  (aset eobj 1 (mapcar (lambda (x) (string-to-number x))
		       (split-string revision-str "\\."))))

(defun EIMIL-make-new-eobj (&optional base-eobj)
  (let ((eobj
	 (if base-eobj
	     (let ((eobj (copy-sequence base-eobj)))
	       (aset eobj 3 (make-vector EIMIL-obarray-size 0)) ; private obarray
	       eobj)
	   (vector nil nil
		   (make-vector EIMIL-obarray-size 0)
		   (make-vector EIMIL-obarray-size 0)
		   nil nil nil nil nil nil nil nil)))
	sym)
    (if base-eobj
	eobj
      ;; Set up system internal symbols
      (setq sym (EIMIL-register-symbol eobj "nop" 'operation t))
      (set sym nil)
      (setq sym (EIMIL-register-symbol eobj "feedback" 'property t))
      (EIMIL-symbol-set-type sym 'mtext)
      (set sym 'mtext)
      (setq sym (EIMIL-register-symbol eobj "current" 'property t))
      (EIMIL-symbol-set-type sym 'bool)
      (set sym 'bool)
      (setq sym (EIMIL-register-symbol eobj "candidates" 'property t))
      (EIMIL-symbol-set-type sym 'mtext)
      (set sym 'mtext)
      (setq sym (EIMIL-register-symbol eobj "selected-candidate" 'property t))
      (EIMIL-symbol-set-type sym 'number)
      (set sym 'number)
      eobj)))

;;
;; copy
;;

(defun EIMIL-copy-function-constants (consts privob pubob)
  (let ((i 0)
	(len (length consts))
	result obj)
    (while (< i len)
      (setq obj (aref consts i)
	    i (1+ i))
      (cond ((EIMIL-symbol-p obj)
	     (setq result (cons (EIMIL-reintern-symbol obj privob pubob)
				result)))
	    ((and (consp obj)
		  (eq (car obj) 'byte-code))
	     (setq result (list
			   (car obj) (nth 1 obj)
			   (EIMIL-copy-function-constants (nth 2 obj) privob pubob))))
	    (t
	     (setq result (cons obj result)))))
    (apply (function vector) (nreverse result))))

(defun EIMIL-copy-funcdef (funcdef privob pubob)
  (make-byte-code
   (compiled-function-arglist funcdef)
   (compiled-function-instructions funcdef)
   (EIMIL-copy-function-constants
    (compiled-function-constants funcdef)
    privob pubob)
   (compiled-function-stack-depth funcdef)))

(defun EIMIL-copy-eobj-1 (eobj pubobarray)
  (setq eobj (copy-sequence eobj))
  (if pubobarray (aset eobj 2 pubobarray)
    (aset eobj 2 (EIMIL-copy-pubobarray (EIMIL-pubobarray eobj))))
  (aset eobj 3 (EIMIL-copy-privobarray
		(EIMIL-privobarray eobj) pubobarray))
  (aset eobj 4 (EIMIL-reintern-symbols
		(EIMIL-inherit-ccdefs eobj)
		(EIMIL-privobarray eobj) pubobarray))
  (aset eobj 5 (EIMIL-reintern-symbol
		(EIMIL-UIdata eobj)
		(EIMIL-privobarray eobj) pubobarray))
  (aset eobj 6 (EIMIL-reintern-symbols
		(EIMIL-commitnotify-ops eobj)
		(EIMIL-privobarray eobj) pubobarray))
  eobj)

(defun EIMIL-copy-obarray-1 (srcob destob pubob)
  (let (sym val neobj slot)
    (mapatoms
     (lambda (x)
       (setq sym (EIMIL-reintern-symbol x destob pubob))
       (cond ((eq 'ccdef (EIMIL-symbol-category x))
	      (set sym (EIMIL-copy-eobj-1 (symbol-value x) pubob)))
	     ((eq 'engine (EIMIL-symbol-category x))
	      (setq val (symbol-value x)
		    neobj (EIMIL-copy-eobj-1 val pubob)
		    slot (assoc (EIMIL-engine-class val) EIMIL-engine-alist))
	      (set sym (funcall (nth 3 slot) neobj)))
	     ((eq 'function (EIMIL-symbol-category x))
	      (set sym (EIMIL-copy-funcdef (symbol-value x) destob pubob)))
	     ((eq 'operation (EIMIL-symbol-category x))
	      (set sym
		   (mapcar
		    (lambda (x)
		      (cons
		       (mapcar
			(lambda (y)
			  (EIMIL-reintern-symbol y destob pubob))
			(car x))
		       (mapcar
			(lambda (y)
			  (EIMIL-reintern-symbol y destob pubob))
			(cdr x))))
		    (symbol-value x))))
	     ((and (eq 'variable (EIMIL-symbol-category x))
		   (eq 'prop (EIMIL-symbol-type x)))
	      (if (symbol-value x)
		  (error "Variable:%S has already had a prop value:%S."
			 x (symbol-value x)))
	      (set sym nil))
	     (t
	      (set sym (symbol-value x))))
       (setplist sym (symbol-plist x)))
     srcob)
    destob))

(defun EIMIL-copy-pubobarray (ob)
  (let ((result (make-vector EIMIL-obarray-size 0)))
    (EIMIL-copy-obarray-1 ob result result)))

(defun EIMIL-copy-privobarray (ob pubobarray)
  (let ((result (make-vector EIMIL-obarray-size 0)))
    (EIMIL-copy-obarray-1 ob result pubobarray)))

(defun EIMIL-copy-eobj (eobj)
  (let ((neobj (EIMIL-copy-eobj-1 eobj nil)))
    (if (EIMIL-engine-name neobj)
	(symbol-value
	 (or (EIMIL-intern-soft neobj (EIMIL-engine-name neobj))
	     (error "Internal error(fail to find the corresponding engine.): %s"
		    (EIMIL-engine-name neobj))))
      neobj)))

;;;
;;; Document template
;;;

;; (<ordering type>
;;   (<element-name> <option> <attr-spec> [<func> <subelements>])...)
(defconst EIMIL-document-template
  '(loose
    (ccdef
     single ((name . nmtoken)
	     (revision . nmtoken)
	     (type . ("abstruct" "concrete")))
     EIMIL-parse-ccdef
     (ordered
      (inherit multiple ((xlink:href . cdata)) EIMIL-parse-inherit nil)
      (interface single nil nil
       (ordered
	(langinfo multiple ((xml:lang . cdata)) EIMIL-parse-langinfo any)
	(decldata multiple ((name . nmtoken) (type . ("number" "char" "mtext" "bool")))
		  EIMIL-parse-decldata nil)
	(declprop multiple ((name . nmtoken) (type . ("number" "char" "mtext" "bool")))
		  EIMIL-parse-declprop nil)
	(declop multiple ((name . nmtoken))
		EIMIL-parse-declop
		(ordered (dependency
			  morethan1
			  ((depend . nmtokens) (affect . nmtokens)))))
	(commitnotify zeroor1 ((op . nmtoken) (flag . ("on" "off")))
		      EIMIL-parse-commitnotify)
	(declexeption multiple ((name . nmtoken))
		      EIMIL-parse-declexception
		      (ordered (message
				morethan1
				((xml:lang . cdata)) nil any)))
	(UIdata morethan1 ((depend . nmtoken))
		EIMIL-parse-UIdata)))
      (engine multiple ((name . nmtoken) (class . cdata))
       EIMIL-parse-engine any)
      ))))

;;;
;;; patterns
;;;

(defconst EIMIL-space " \n\t")
(defconst EIMIL-space-regexp (concat "[" EIMIL-space "]"))

(defconst EIMIL-nmtoken-regexp 
  (concat "\\`" EIMIL-space-regexp "*"
	  "\\([-A-z0-9._:]+\\)"
	  EIMIL-space-regexp "*\\'"))

(defconst EIMIL-nmtokens-regexp 
  (concat "\\`" EIMIL-space-regexp "*"
	  "[-A-z0-9._:]+"
	  "\\(" EIMIL-space-regexp "+"
	  "[-A-z0-9._:]+" "\\)*"
	  EIMIL-space-regexp "*\\'"))

(defun EIMIL-start-tag-attr-regexp (plus)
  (concat "\\(\\("
	  EIMIL-space-regexp "[^" EIMIL-space "=>]+"
	  EIMIL-space-regexp "?=" EIMIL-space-regexp
	  "?\\('[^']*'\\|\"[^\"]*\"\\)\\)"
	  (if plus "+" "*")
	  "\\)"))

(defconst EIMIL-any-start-tag-regexp
  (concat "<\\([-A-z0-9._:]+\\)"
	  (EIMIL-start-tag-attr-regexp nil)
	  EIMIL-space-regexp "?" 
	  "/?>"))

(defconst EIMIL-any-end-tag-regexp
  (concat "</\\([-A-z0-9._:]+\\)" EIMIL-space-regexp "?>"))

(defconst EIMIL-any-s-or-e-tag-regexp
  (concat "</?\\([-A-z0-9._:]+\\)"
	  (EIMIL-start-tag-attr-regexp nil)
	  EIMIL-space-regexp "?" 
	  "/?>"))

(defun EIMIL-strip-comment ()
  (save-excursion
    (goto-char (point-min))
    (let (pt)
      (while (search-forward "<!--" nil t)
	(setq pt (match-beginning 0))
	(if (not (search-forward "--" nil t))
	    (error "Fail to find the end of comment."))
	(if (not (eq (following-char) ?>))
	    (error "In comments, `--' sequence is not allowd."))
	(delete-region pt (1+ (point)))))))
    

(defun EIMIL-construct-stag-regexp (spec)
  (if (eq spec 'any)
      EIMIL-any-start-tag-regexp
    (let ((name (car spec))
	  (attrs (nth 2 spec))
	  (sub (nth 4 spec)))
      (concat "<\\(" (symbol-name name) "\\)"
	      (if attrs
		  (EIMIL-start-tag-attr-regexp t))
	      EIMIL-space-regexp "?" 
	      (if sub ">" "/>")))))

(defun EIMIL-construct-etag-regexp (spec)
  (if (eq spec 'any)
      EIMIL-any-end-tag-regexp
    (let ((name (car spec)))
      (concat "</" (symbol-name name)
	      EIMIL-space-regexp "?>" ))))

(defun EIMIL-construct-s-or-e-tag-regexp (spec)
  (if (eq spec 'any)
      EIMIL-any-s-or-e-tag-regexp
    (let ((name (car spec))
	  (attrs (nth 2 spec))
	  (sub (nth 4 spec)))
      (concat "</?\\(" (symbol-name name) "\\)"
	      (if attrs
		  (EIMIL-start-tag-attr-regexp nil))
	      EIMIL-space-regexp "?" 
	      (if sub ">" "/>")))))

;;;
;;; reference.
;;;

(defun EIMIL-resolve-reference (str)
  (let ((start 0)
	(len (length str))
	(result "")
	ref result)
    (while (and (> len start)
		(string-match "&\\([^;]+\\);" str start))
      (setq result (concat result (substring str start (match-beginning 0)))
	    start (match-end 0)
	    ref (match-string 1 str))
      (cond ((string-match "\\`#\\([0-9]+\\)\\'" ref)
	     (setq result (concat
			   result
			   (char-to-string
			    (ucs-to-char (string-to-number (match-string 1 ref) 10))))))
	    ((string-match "\\`#x\\([0-9a-fA-F]+\\)\\'" ref)
	     (setq result (concat
			   result
			   (char-to-string
			    (ucs-to-char (string-to-number (match-string 1 ref) 16))))))
	    (t
	     (error "Cannot resolve reference %s." ref))))
    (concat result (substring str start))))

;;;
;;; moving in or narrowing a buffer
;;;

(defun EIMIL-search-tag ()
  (prog1
      (search-forward "<" nil t)
    (backward-char)))

(defun EIMIL-search-stag (spec)
    (re-search-forward (EIMIL-construct-stag-regexp spec) nil t))

(defun EIMIL-search-etag (spec)
  (if (eq spec 'any)
    (re-search-forward (EIMIL-construct-etag-regexp spec) nil t)
    (let ((depth 1)
	  (re (EIMIL-construct-s-or-e-tag-regexp spec))
	  str)
      (while
	  (progn
	    (if (not (re-search-forward re nil t))
		(error "Cannot find End tag!"))
	    (if (eq (aref (match-string 0) 1) ?/)
		(setq depth (1- depth))
	      (setq depth (1+ depth)))
	    (> depth 0)))))
  (goto-char (match-beginning 0)))

;;;
;;; Parse Attribute.
;;;

(defun EIMIL-parse-nmtoken (str)
  (if (string-match EIMIL-nmtoken-regexp
		    (EIMIL-resolve-reference str))
      (match-string 1 str)
    (error "Invalid NMTOKEN:%s" str)))

(defun EIMIL-parse-nmtokens (str)
  (let* ((start 0)
	 (rstr (EIMIL-resolve-reference str))
	 (len (length rstr))
	 result)
    (while (> len start)
      (if (string-match EIMIL-nmtoken-regexp rstr start)
	  (setq result (cons (match-string 1 rstr) result)
		start (match-end 0))
	(error "Invalid attribute string:%s" str)))
    result))

(defun EIMIL-parse-attrs-string (str)
  (let ((start 0)
	(len (length str))
	nstr
	vstr
	result)
    (while (> len start)
      (if (string-match 
	   (concat "\\([-A-z0-9._:]+\\)"
		   EIMIL-space-regexp "?=" EIMIL-space-regexp
		   "?\\('[^']*'\\|\"[^\"]*\"\\)")
	   str start)
	  (setq nstr (match-string 1 str)
		vstr (match-string 2 str)
		result (cons (cons nstr (substring vstr 1 (1- (length vstr))))
			     result)
		start (match-end 0))
	(error "Invalid attribute string:%s" str)))
    result))

(defun EIMIL-parse-attrs (str attrspec)
  (let ((attrs (EIMIL-parse-attrs-string str))
	template attr result sym)
    (while (setq attr (car attrs))
      (setq sym (intern (car attr))
	    template (cdr (assq sym attrspec)))
      (if (null template)
	  (error "%s is not valilid attribute name" (car attr)))
      (cond ((eq template 'nmtoken)
	     (setq result (cons (list sym (EIMIL-parse-nmtoken (cdr attr)))
				result)))
	    ((eq template 'nmtokens)
	     (setq result (cons (cons sym (EIMIL-parse-nmtokens (cdr attr)))
				result)))
	    ((eq template 'cdata)
	     (setq result (cons (list sym (EIMIL-resolve-reference
					   (cdr attr)))
				result)))
	    ((listp template)
	     (while (and (not (string= (cdr attr) (car template)))
			 (setq template (cdr template))))
	     (if (null template)
		 (error "attribute %s has an invalid value(%s)."
			(car attr) (cdr attr)))
	     (setq result (cons (list sym (cdr attr))
				result))))
      (setq attrs (cdr attrs)))
    result))

;;;
;;; Parse Elements.
;;;

(defun EIMIL-parse-element-1 (eobj etemp attrstr)
  (let ((pt (point))
	(attrs (EIMIL-parse-attrs attrstr (nth 2 etemp)))
	(func (nth 3 etemp))
	(sub (nth 4 etemp))
	pt-end)
    (if (and sub
	     (not (EIMIL-search-etag etemp)))
	(error "Cannot find the expected end tag."))
    (setq pt-end (match-end 0))
    (save-excursion
      (save-restriction
	(narrow-to-region pt (point))
	(goto-char (point-min))
	(if func (funcall func eobj attrs))
	(goto-char (point-min))
	(if (consp sub) (EIMIL-parse-element eobj sub))))
    (goto-char pt-end)))

(defun EIMIL-parse-element (eobj doctemp)
  (let ((ot (car doctemp))
	(etemps (cdr doctemp))
	et etemp pt nums num sym)
    (cond ((eq ot 'ordered)
	   (while (setq etemp (car etemps))
	     (setq etemps (cdr etemps)
		   num 0
		   et (nth 1 etemp))
	     (while
		 (progn
		   (if (not (and (EIMIL-search-tag)
				 (setq pt (point))
				 (EIMIL-search-stag etemp)
				 (= (match-beginning 0) pt)))
		        nil
		     (EIMIL-parse-element-1 eobj etemp (match-string 2))
		     (setq num (1+ num))
		     (or (eq et 'multiple)
			 (eq et 'morethan1)))))
	     (if (or (and (eq et 'morethan1)
			  (< num 1))
		     (and (eq et 'single)
			  (/= num 1))
		     (and (eq et 'zeroor1)
			  (> num 2)))
		 (error "The number of element %S is invalid."
			etemp))))
	  (t
	   (while (EIMIL-search-stag 'any)
	     (setq sym (intern (match-string 1)))
	     (if (null (setq etemp (assq sym etemps)))
		 (error "Invalid start tag:%S." sym))
	     (EIMIL-parse-element-1 eobj etemp (match-string 2))
	     (setq et (assq sym nums))
	     (if et (setcdr et (1+ (cdr et)))
	       (setq nums (cons (cons sym 1) nums))))
	   ;; check the number of elements.
	   (while (setq etemp (car etemps))
	     (setq num (or (cdr (assq (car etemp) nums)) 0)
		   et (nth 1 etemp))
	     (if (or (and (eq et 'morethan1)
			  (< num 1))
		     (and (eq et 'single)
			  (/= num 1))
		     (and (eq et 'zeroor1)
			  (> num 2)))
		 (error "The number of element %S is invalid."
			etemp))
	     (setq etemps (cdr etemps)))))
    nil))

;;;
;;; parser handlers
;;;

(defun EIMIL-parse-ccdef (eobj attrs)
  (EIMIL-set-name
   eobj
   (nth 1 (or (assq 'name attrs)
	      (error "ccdef element must have `name' attribute."))))
  (EIMIL-set-revision
   eobj
   (nth 1 (or (assq 'revision attrs)
	      (error "ccdef element must have `revision' attribute.")))))

(defun EIMIL-parse-inherit (eobj attrs)
  (message "Inherit:%S" attrs))

(defun EIMIL-parse-langinfo (eobj attrs)
  ;;TODO
  )

(defun EIMIL-parse-decldata (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "decldata element must have `name' attribute.")))
	       'variable t))
	(type (intern
	       (nth 1 (or (assq 'type attrs)
			  (error "decldata element must have `type' attribute."))))))
    (EIMIL-symbol-set-type name type)
    (set name nil)))

(defun EIMIL-parse-declprop (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "declprop element must have `name' attribute.")))
	       'property t))
	(type (intern
	       (nth 1 (or (assq 'type attrs)
			  (error "declprop element must have `type' attribute."))))))
    (EIMIL-symbol-set-type name type)
    (set name type)))

(defun EIMIL-parse-dependency (eobj attrs)
  (cons
   (mapcar
    (lambda (x)
      (EIMIL-check-symbol eobj x 'operation t))
    (cdr (or (assq 'depends attrs)
	     (error "dependency element must have `depends' attribute."))))
   (mapcar
    (lambda (x)
      (EIMIL-check-symbol eobj x 'operation t))
    (cdr (or (assq 'affects attrs)
	     (error "dependency element must have `affects' attribute."))))))

(defun EIMIL-parse-declop (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "declop element must have `name' attribute.")))
	       'operation t)))
    (set name nil)
    (EIMIL-parse-element
     eobj
     '(ordered (dependency
		morethan1
		((depend . nmtokens) (affect . nmtokens))
		(lambda (eobj attrs)
		  (setcdr (symbol-value name)
			  (cons
			   (EIMIL-parse-dependency eobj attrs)
			   (cdr (symbol-value name))))))))))

(defun EIMIL-parse-commitnotify (eobj attrs)
  (let ((op (cdr (or (assq 'op attrs)
		     (error "commitnotify element must have `op' attribute."))))
	(flag
	 (nth 1 (or (assq 'flag attrs)
		    (error "commitnotify element must have `flag' attribute.")))))
    (mapcar
     (if (string= flag "on")
	 (lambda (x)
	   (EIMIL-add-commitnotify-op
	    eobj (EIMIL-check-symbol eobj x 'operation t)))
       (lambda (x)
	 (EIMIL-remove-commitnotify-op
	  eobj (EIMIL-check-symbol eobj x 'operation t))))
     op)))

(defun EIMIL-extract-message ()
  (let (st end)
    (setq st (1- (re-search-forward (concat "[^" EIMIL-space "]"))))
    (goto-char (point-max))
    (setq end (1+ (re-search-backward (concat "[^" EIMIL-space "]"))))
    (EIMIL-resolve-reference (buffer-substring st end))))

(defun EIMIL-parse-declexception (eobj attrs)
  (let ((name (EIMIL-register-symbol
	       eobj
	       (nth 1 (or (assq 'name attrs)
			  (error "declexception element must have `name' attribute.")))
	       'exception t)))
    (set name nil)
    (EIMIL-parse-element
     eobj
     '(ordered (message
		morethan1
		((xml:lang . cdata))
		(lambda (eobj attrs)
		  (setcdr (symbol-value name)
			  (cons
			   (cons
			    (nth 1 (or (assq 'xml:lang attrs)
				       (error
					"message element must have `xml:lang' attribute.")))
			    (EIMIL-extract-message))
			   (cdr (symbol-value name))))))))))

(defun EIMIL-parse-UIdata (eobj attrs)
  (EIMIL-set-UIdata
   eobj
   (EIMIL-check-symbol
    eobj
    (nth 1 (or (assq 'depend attrs)
	       (error "UIdata element must have `depend' attribute.")))
    'variable t)))

(defun EIMIL-parse-engine (eobj attrs)
  (let ((class (nth 1 (or (assq 'class attrs)
			  (error "engine element must have `class' attribute."))))
	(name (nth 1 (or (assq 'name attrs)
			  (error "engine element must have `name' attribute."))))
	neobj slot sym)
    (setq slot (assoc class EIMIL-engine-alist)
	  sym (EIMIL-register-symbol eobj name 'engine t))
    (if (null slot)
	(message "Unknown engine class(%s) is detected.  I'll ignore it." class)
      (setq neobj (EIMIL-make-new-eobj eobj))
      (set sym neobj)
      (funcall (nth 1 slot) neobj sym)
      (EIMIL-set-engine-class-and-name neobj class name)
      (EIMIL-set-event-handler neobj (nth 2 slot))
      (EIMIL-set-private eobj (cons (cons name neobj)
				    (EIMIL-private eobj))))))

;;;
;;; EIMIL Engine Interface
;;;

(defsubst EIMIL-get-handler (eobj kind)
  (cdr (assq kind (EIMIL-handler-alist eobj))))

(defun EIMIL-toggle-preedit-text (eobj flag)
  (funcall (or (EIMIL-get-handler eobj 'toggle-preedit-text)
	       (function ignore)) eobj flag))

(defun EIMIL-toggle-lookup-choice (eobj flag)
  (funcall (or (EIMIL-get-handler eobj 'toggle-lookup-choice)
	       (function ignore)) eobj flag))

(defun EIMIL-commit-string (eobj str)
  (funcall (or (EIMIL-get-handler eobj 'commit-string)
	       (function ignore)) eobj str))

;;;
;;; EIMIL API
;;;

;; for engine provider.
(defun EIMIL-register-engine (class parser evhandler copy)
  (let ((slot (assoc class EIMIL-engine-alist)))
    (if slot (setcdr slot (list parser evhandler copy))
      (setq EIMIL-engine-alist
	    (cons (list class parser evhandler copy)
		  EIMIL-engine-alist)))))

;; UI-handler prototype
;; (lambda (command mtext))
;;   command: update-preedit-text
;;            toggle-preedit-text
;;            update-lookup-choice
;;            toggle-lookup-choice
;;            commit-string

(defun EIMIL-register-handler (eobj kind handler)
  (if (memq kind '(update-preedit-text
		   toggle-preedit-text
		   update-lookup-choice
		   toggle-lookup-choice
		   commit-string))
      (EIMIL-set-handler-alist
       eobj (cons (cons kind handler) (EIMIL-handler-alist eobj)))
    (error "Invalid kind:%S" kind)))

(defun EIMIL-get-preedit-text (eobj &optional force)
  ;;; format....
  (symbol-value (EIMIL-UIdata eobj)))

(defun EIMIL-get-lookup-choice (eobj &optional force)
  )


(defun EIMIL-delegate (eobj emev)
  (prog1
      (funcall (EIMIL-event-handler eobj) eobj emev)
    ;; TODO: UI update management
    nil))

(defun EIMIL-convert-mtext (mtext sym)
  (let* ((text (copy-sequence mtext))
	 (len (length mtext))
	 (beg (text-property-not-all 0 len sym nil mtext))
	 end slots ext)
    (while beg
      (setq end (or (next-single-property-change beg sym mtext)
		    len))
      (setq slots (reverse (get-text-property beg sym mtext)))
      (while (setq ext (car slots))
	(put-text-property (aref ext 1) (aref ext 2) sym (aref ext 3) text)
	(setq slots (cdr slots)))
      (setq beg end))
    text))

(defun EIMIL-save (eobj &optional stream))
(defun EIMIL-load (eobj &optional stream))

(defun EIMIL-parse-buffer ()
  (let ((eobj (EIMIL-make-new-eobj)))
    ;;(set-window-buffer w (current-buffer))
    (EIMIL-strip-comment)
    (EIMIL-parse-element eobj EIMIL-document-template)
    (EIMIL-private eobj)))

(defun EIMIL-parse-file (filename)
  (save-excursion
    (let ((buf
	   (generate-new-buffer " *EIMIL-tmp*"))
	  eobjsalist)
      (set-buffer buf)
      (insert-file-contents filename)
      (setq eobjsalist (EIMIL-parse-buffer))
      (kill-buffer buf)
      eobjsalist)))

(provide 'EIMIL)

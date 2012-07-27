;; Autoload file for smalltalk-mode

;; duplicate zip files' setup for star files or fall back on
;; archive-mode, which scans file contents to determine type so is
;; safe to use
(push (cons "\\.star\\'"
	    (catch 'archive-mode
	      (dolist (mode-assoc auto-mode-alist 'archive-mode)
		(and (string-match (car mode-assoc) "Starfile.zip")
		     (functionp (cdr mode-assoc))
		     (throw 'archive-mode (cdr mode-assoc))))))
      auto-mode-alist)

(push '("\\.st\\'" . smalltalk-mode) auto-mode-alist)

(push "\\.star\\'" inhibit-first-line-modes-regexps)

(autoload 'smalltalk-mode "/usr/local/share/emacs/site-lisp/smalltalk-mode.elc" "" t)
(autoload 'gst "/usr/local/share/emacs/site-lisp/gst-mode.elc" "" t)


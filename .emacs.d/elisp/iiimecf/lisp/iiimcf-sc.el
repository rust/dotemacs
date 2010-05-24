;;; iiimcf-sc.el --- IIIMCF server control input method.
;;;

;; Copyright (C) 2000-2002 MIYASHITA Hisashi
;; Copyright (C) 2004-2005 Hiroshi Miura, NTT DATA Intellilink Co. 

;; Author: MIYASHITA Hisashi <himi@li18nux.org>
;; Maintainer: Hiroshi Miura <miura@da-cha.org>

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

;;; Commentary:
;; This module provides a simple input method
;; under the server-side control via IIIM.

;;; Code:

(require 'iiimcf)
(require 'iiimcf-UI)

;;; version.

(defconst iiimcf-server-control-version "0.7 (Edo)")

;;; Customizable options.

(defgroup iiimcf-server-control nil
  "*IIIMCF server control input method."
  :tag "IIIMCF Server Control Input Method"
  :group 'applications
  :group 'i18n)

(defcustom iiimcf-server-control-default-port 9010
  "*Default port number for IIIM server if proto is `tcp'."
  :tag "Default port for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'integer)

;;(defcustom iiimcf-server-control-default-proto "tcp"
;;  "*Default protocol socket domain for IIIM server."
;;  :tag "Default protocol socket domain for IIIMCF Server Control"
;;  :group 'iiimcf-server-control :type 'string)

(defcustom iiimcf-server-control-hostlist (list (concat "/tmp/.iiim-" (user-login-name) "/:1.0"))
  "*A list of IIIM Server hosts.
Currently, only the first server in the list will be used.  It
should be the form of `(proto:)server(:port)', where PROTO should
be either `tcp' or `unix'.  If `unix' is specified as PROTO, then
the SERVER specifies socket path and the PORT specification will
be ignored.  If PORT is not specified, the value of
`iiimcf-server-control-default-port' will be used instead.

Example: * unix:/var/run/iiim/.iiimp-unix/9010
         * tcp:iiimserver.localdomain:9999
         * localhost (which means tcp:localhost:9010)"
  :tag "Hostlist for IIIMCF Server Control"
  :group 'iiimcf-server-control :type '(repeat string))

(defcustom iiimcf-server-control-username
  (concat (user-login-name) "@" (system-name))
  "*Username passed to IIIM server."
  :tag "Username for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'string)

(defcustom iiimcf-server-control-default-language nil
  "*Use this language by default.
If nil, use the first candidate sent by a server."
  :tag "Default language for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'string)

(defcustom iiimcf-server-control-default-input-method nil
  "*Use this input method by default.
If nil, don't apply input method preference to a server."
  :tag "Default input method for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'string)

(defcustom iiimcf-server-control-async-invocation-p t
  "*Receives and responds incomming messages asynchronously, if non-nil."
  :tag "Aaync invocation flag for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'boolean)

(defcustom iiimcf-server-control-send-region nil
  "*Send selected region to the IIIM server and replace the result, if non-nil.
Marker for the region must at the same line."
  :tag "Send region flag for IIIMCF Server Control"
  :group 'iiimcf-server-control :type 'boolean)

(defvar iiimcf-server-control-keyboard-translate-table nil
  "Keyboard translation table exclusively used for preedit-mode.
If `nil' is set, then the table for preedit wiil be set to nil.
If `t', then the original table will be preserved.  Otherwise,
the specified value would be set for the translate-table.")

;; keyboards
(defvar iiimcf-server-control-keyboard-list
  '(("ascii" . iiimcf-ascii-keycode-spec-alist)
    ("kana" . iiimcf-kana-keycode-spec-alist)))

;;; internal variables

(defvar iiimcf-server-control-enable nil)
(make-variable-buffer-local 'iiimcf-server-control-enable)
(put 'iiimcf-server-control-enable 'permanent-local t)
(defvar iiimcf-server-control-com-id nil)
(defvar iiimcf-server-control-im-id nil)
(defvar iiimcf-server-control-ic-id nil)
(make-variable-buffer-local 'iiimcf-server-control-ic-id)
(put 'iiimcf-server-control-ic-id 'permanent-local t)
(defvar iiimcf-server-control-dynamic-event-flow-p nil)
(defvar iiimcf-server-control-own-event-loop nil)
(defvar iiimcf-server-control-event-loop-uic nil)

(defvar iiimcf-server-control-focused-ic-id nil)

;;;
;;; keymap
;;;

;; keymap for initial state.
(defvar iiimcf-server-control-initial-state-keymap
  (let ((map (make-keymap))
	(i 32))
    (while (< i 127)
      (define-key map (char-to-string i)
	'iiimcf-server-control-keyforward)
      (setq i (1+ i)))
    (setq i 65)
    (while (< i 91)
      (define-key map `[(alt ,i)]
	'iiimcf-server-control-keyforward)
      (setq i (1+ i)))
    (define-key map [prior] 'iiimcf-server-control-keyforward)
    (define-key map [next] 'iiimcf-server-control-keyforward)
    ;; For our own features.
    (define-key map "\C-c\C-\\\C-l" 'iiimcf-server-control-change-language)
    (define-key map "\C-c\C-\\\C-i" 'iiimcf-server-control-change-input-method)
    (define-key map "\C-c\C-\\\C-s" 'iiimcf-server-control-switch-le)
    (define-key map "\C-c\C-\\\C-k" 'iiimcf-server-control-switch-keyboard)
    map))

;; keymap for preedit state
(defvar iiimcf-server-control-preedit-state-keymap
  (let ((map (make-keymap))
	(i 0))
    (while (<= i 127)
      (if (/= i ?\x1B) ; skip ESC
	  (define-key map (char-to-string i)
	    'iiimcf-server-control-keyforward))
      (setq i (1+ i)))
    (setq i 65)
    (while (< i 91)
      (define-key map `[(alt ,i)]
	'iiimcf-server-control-keyforward)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i)
	'iiimcf-server-control-keyforward)
      (setq i (1+ i)))

    (dolist (fkey '(delete backspace return up down left right
		    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
		    home end print prior next pause insert
		    zenkaku-hankaku))
      (define-key map `[,fkey] 'iiimcf-server-control-keyforward)
      (define-key map `[(shift ,fkey)] 'iiimcf-server-control-keyforward)
      (define-key map `[(control ,fkey)] 'iiimcf-server-control-keyforward)
      (define-key map `[(shift control ,fkey)] 'iiimcf-server-control-keyforward))
    (set-keymap-parent map iiimcf-server-control-initial-state-keymap)
    map)
  "Keymap used for forwarding keyevents to IIIM server side in preedit state.")

;;; IIIMCF event handler.

(defconst iiimcf-server-control-com-level-handler-alist
  '((iiimcf-server-control-setup-event-flow-mode . iiimp-im-register-trigger-keys)
    (iiimcf-server-control-maintain-language-list . iiimp-im-connect-reply)
    (iiimcf-server-control-maintain-input-method-list . iiimp-im-setimvalues)))

(defconst iiimcf-server-control-ic-level-handler-alist
  '((iiimcf-server-control-trigger-notify . iiimp-im-trigger-notify)
    (iiimcf-server-control-register-hotkeys . iiimp-im-register-hotkeys)
    (iiimcf-server-control-select-hotkey-profile . iiimp-im-select-hotkey-profile)
    (iiimcf-server-control-maintain-preedit-text . iiimp-im-preedit-draw)
    (iiimcf-server-control-commit-string . iiimp-im-commit-string)
    (iiimcf-server-control-forward-event . iiimp-im-forward-event)
    (iiimcf-server-control-forward-event . iiimp-im-forward-event-with-operations)
    (iiimcf-server-control-draw-lookup-choice . iiimp-im-lookup-choice-draw)
    (iiimcf-server-control-clear-lookup-choice . iiimp-im-lookup-choice-done)
    (iiimcf-server-control-draw-status . iiimp-im-status-draw)
    (iiimcf-server-control-draw-aux-data . iiimp-im-aux-draw)))

(defun iiimcf-server-control-register-handlers
  (alist com-id &optional im-id ic-id)
  (let (elem)
    (while (setq elem (car alist))
      (iiimcf-register-handler
       (car elem)
       (if (and im-id ic-id)
	   (list (cdr elem) com-id im-id ic-id)
	 (list (cdr elem) com-id)))
      (setq alist (cdr alist)))))

(defun iiimcf-server-control-remove-handlers (alist)
  (while alist
    (iiimcf-remove-handler
     (car (car alist)) nil)
    (setq alist (cdr alist))))

;;;

(defun iiimcf-server-control-parse-hostname (hostname)
  (let (proto host port)
    (if (string-match 
         (concat "^\\(?:\\(tcp:\\)?\\([a-zA-Z0-9\.-]+\\)\\(:[0-9]+\\)\\)\\|"
                 "\\(?:\\(unix:\\)?\\([a-zA-Z0-9\./:-]+\\)\\)$") hostname)
        (progn
          (if (match-string 2 hostname)
              (setq proto "tcp"
                    host (match-string 2 hostname)
                    port (match-string 3 hostname)
                    port (if (null port) iiimcf-server-control-default-port
                           (string-to-number (substring port 1))))
            (setq proto "unix"
                  host (match-string 5 hostname)))
          (list proto host port))
      (error "server name %s is not valid!" hostname))))

(defun iiimcf-server-control-setup-event-flow-mode (mes)
  (setq iiimcf-server-control-dynamic-event-flow-p t))

(defun iiimcf-server-control-async-handler (com-id)
  (condition-case err
      (iiimcf-message-manager com-id nil)
    (iiimp-fatal
     (iiimcf-server-control-shutdown)
     (signal (car err) (cdr err)))))

(defun iiimcf-server-control-get-ic-attribute (im-id)
  (let ((lang iiimcf-server-control-default-language)
	(im iiimcf-server-control-default-input-method))
    (if (null lang)
	(setq lang
	      (car
	       (iiimcf-com-id-get
		(iiimp-im-id-to-com-id im-id)
		'language-list))))
    (nconc
     (list (cons 'input-language lang))
     (if im (list (cons 'input-method im)) nil)
     )))

(defun iiimcf-server-control-prepare-uic (im-id &optional initp)
  (let (uic ic-id)
    (if (or (not iiimcf-server-control-ic-id)
	    (iiimcf-UI-preedit-text-flag
	     (setq uic (iiimcf-ic-id-get
			iiimcf-server-control-ic-id
			'iiimcf-server-control-uic))))
	;; create new ic-id
	(progn
	  (setq ic-id (iiimcf-create-ic
		       im-id (iiimcf-server-control-get-ic-attribute im-id))
		uic (iiimcf-UI-create-context nil nil ic-id))
	  (iiimcf-ic-id-put ic-id 'iiimcf-server-control-uic uic)
	  (iiimcf-UI-set-keymap
	   uic iiimcf-server-control-preedit-state-keymap)
	  (if initp
	      (setq iiimcf-server-control-ic-id ic-id)))
      ;; reuse iiimcf-server-control-ic-id
      (if initp
	  ;; do nothing because ic-id is already inited.
	  nil
	(if uic
	    (iiimcf-UI-move-marker uic)
	  (setq uic (iiimcf-UI-create-context
		     nil nil iiimcf-server-control-ic-id))
	  (iiimcf-ic-id-put iiimcf-server-control-ic-id
			    'iiimcf-server-control-uic
			    uic)
	  (iiimcf-UI-set-keymap
	   uic iiimcf-server-control-preedit-state-keymap))))
    uic))

(defsubst iiimcf-server-control-current-uic (im-id)
  (or (iiimcf-UI-current-context)
      (iiimcf-server-control-prepare-uic im-id)))

(defsubst iiimcf-server-control-message-uic (mes)
  (iiimcf-ic-id-get (iiimp-message-ic-id mes)
		    'iiimcf-server-control-uic))

(defun iiimcf-server-control-delete-ic (ic-id)
  (let ((uic (iiimcf-ic-id-get ic-id 'iiimcf-server-control-uic))
	buf obuf)
    (iiimcf-destroy-ic ic-id)
    (if uic
	(progn
	  (setq buf (marker-buffer (iiimcf-UI-marker uic)))
	  (if (bufferp buf)
	      (progn
		(setq obuf (current-buffer))
		(set-buffer buf)
		(if (equal iiimcf-server-control-ic-id ic-id)
		    (setq iiimcf-server-control-ic-id nil))
		(set-buffer obuf)))
	  (iiimcf-UI-destroy-context uic)))))

(defun iiimcf-server-control-release-ic (ic-id)
  (let ((uic (iiimcf-ic-id-get ic-id 'iiimcf-server-control-uic)))
    (if (equal ic-id iiimcf-server-control-ic-id)
	(if uic
	    (progn
	      (iiimcf-UI-toggle-preedit uic nil)
	      (iiimcf-UI-toggle-lookup-choice uic nil)
	      ;;(iiimcf-UI-toggle-status uic nil)
	      ))
      (iiimcf-server-control-delete-ic ic-id))))

(defun iiimcf-server-control-delete-buffer-ic ()
  (if iiimcf-server-control-ic-id
      (iiimcf-server-control-delete-ic
       iiimcf-server-control-ic-id)))
(add-hook 'kill-buffer-hook
	  (function iiimcf-server-control-delete-buffer-ic))

(defun iiimcf-server-control-dispatch-emacs-event
  (events &optional buf save-excursion-p)
  (let ((iiimcf-server-control-enable nil))
    (iiimcf-UI-dispatch-emacs-event events buf save-excursion-p)))

(defun iiimcf-server-control-setup-com-id ()
  (if iiimcf-server-control-com-id
      (iiimcf-server-control-shutdown))
  (setq iiimcf-server-control-com-id
	(apply
	 (function iiimcf-connect-com)
	 (iiimcf-server-control-parse-hostname
	  (car iiimcf-server-control-hostlist))))
  (iiimcf-server-control-register-handlers
   iiimcf-server-control-com-level-handler-alist
   iiimcf-server-control-com-id)
  (if iiimcf-server-control-async-invocation-p
      (iiimp-enable-async-invocation
       iiimcf-server-control-com-id
       (function iiimcf-server-control-async-handler))))

(defun iiimcf-server-control-setup-im-id ()
  (setq iiimcf-server-control-im-id
	(iiimcf-connect-im
	 iiimcf-server-control-com-id
	 iiimcf-server-control-username
	 (format "IIIMCF-SC/%s"
		 iiimcf-server-control-version)))
  (iiimcf-server-control-register-handlers
   iiimcf-server-control-ic-level-handler-alist
   iiimcf-server-control-com-id
   iiimcf-server-control-im-id))

(defun iiimcf-server-control-setup ()
  (if (and iiimcf-server-control-com-id
	   (iiimp-check-channel-connection
	    iiimcf-server-control-com-id))
      nil
    (iiimcf-server-control-setup-com-id))
  (if iiimcf-server-control-im-id
      nil
    (iiimcf-server-control-setup-im-id))
  (iiimcf-server-control-prepare-uic
   iiimcf-server-control-im-id t))

(defun iiimcf-server-control-ic-shutdown ()
  (iiimcf-map-all-ics
   (function iiimcf-server-control-delete-ic)
   iiimcf-server-control-im-id)
  (setq iiimcf-server-control-ic-id nil))

(defun iiimcf-server-control-im-shutdown ()
  (if iiimcf-server-control-im-id
      (progn
	(iiimcf-disconnect-im
	 iiimcf-server-control-im-id)
	(setq iiimcf-server-control-im-id nil))))

(defun iiimcf-server-control-shutdown ()
  (inactivate-input-method)
  (iiimcf-server-control-ic-shutdown)
  (iiimcf-server-control-im-shutdown)
  (if iiimcf-server-control-com-id
      (progn
	(iiimcf-destroy-com iiimcf-server-control-com-id)
	(setq iiimcf-server-control-com-id nil)))
  (setq iiimcf-server-control-dynamic-event-flow-p nil)
  (iiimcf-server-control-remove-handlers
   iiimcf-server-control-com-level-handler-alist)
  (iiimcf-server-control-remove-handlers
   iiimcf-server-control-ic-level-handler-alist))

(defun iiimcf-server-control-inactivate ()
  "Inactivate IIIMCF server control input method."
  (interactive)
  (iiimcf-server-control-activate -1))

(defun iiimcf-server-control-remove-minor-mode-map ()
  (let ((islot (assq 'iiimcf-server-control-enable
		     minor-mode-map-alist)))
    (if islot
	(setq minor-mode-map-alist
	      (delq islot minor-mode-map-alist)))))

(defun iiimcf-server-control-register-minor-mode-map ()
  (iiimcf-server-control-remove-minor-mode-map)
  (setq minor-mode-map-alist
	(cons (cons 'iiimcf-server-control-enable
		    iiimcf-server-control-initial-state-keymap)
	      minor-mode-map-alist)))

(defun iiimcf-server-control-activate (&optional arg)
  (if (and arg
	  (< (prefix-numeric-value arg) 0))
      ;; inactivate
      (unwind-protect
	  (progn
	    (setq describe-current-input-method-function nil)
	    (if (and (iiimp-check-channel-connection
		      iiimcf-server-control-com-id)
		     iiimcf-server-control-ic-id
		     ;; do not cast iiimcf-notify-trigger if triggered by server.
		     (eq this-command 'toggle-input-method))
		(iiimcf-notify-trigger
		 iiimcf-server-control-ic-id
		 nil))
	    (iiimcf-UI-toggle-status
	     (iiimcf-ic-id-get
	      iiimcf-server-control-ic-id
	      'iiimcf-server-control-uic) nil)
	    (kill-local-variable 'input-method-function)
	    ;;(run-hooks 'iiimcf-server-control-inactivate-hook)
	    )
	(setq iiimcf-server-control-enable nil))

    ;; activate
    (setq iiimcf-server-control-enable t)
    (iiimcf-server-control-setup)
    (setq inactivate-current-input-method-function
	  'iiimcf-server-control-inactivate)
    (setq describe-current-input-method-function
	  'iiimcf-server-control-help)
    ;; focus
    (setq iiimcf-server-control-focused-ic-id
          iiimcf-server-control-ic-id)
    (iiimcf-set-icfocus iiimcf-server-control-ic-id)
    ;; trigger IC.
    (iiimcf-notify-trigger iiimcf-server-control-ic-id t)
    (iiimcf-UI-toggle-status
     (iiimcf-ic-id-get
      iiimcf-server-control-ic-id
      'iiimcf-server-control-uic) t)
    (make-local-variable 'input-method-function)
    (setq input-method-function
	  (function iiimcf-server-control-keyforward))

    ;; inactivate the current input method also in minibuffers
    ;; before exiting.
    (if (eq (selected-window) (minibuffer-window))
	(add-hook 'minibuffer-exit-hook
		  (function
		   iiimcf-server-control-exit-from-minibuffer)))
    ;;(run-hooks 'iiimcf-server-control-activate-hook)
    (iiimcf-server-control-register-minor-mode-map)
    ;; send region and remove it. ;; NOT YET TESTED.
    (when (and iiimcf-server-control-send-region mark-active)
      (let ((mark (mark-marker)) str)
	(when (= (line-number-at-pos (maker-position mark))
		 (line-number-at-pos (point)))
	  (setq str (buffer-substring (mark-position mark) (point)))
	  (delete-region (mark-position mark) (point))
	  (condition-case err
	      (iiimcf-forward-event iiimcf-server-control-ic-id str)
	    (iiimp-fatal
	     (iiimcf-server-control-shutdown)
	     (signal (car err) (cdr err)))))))
    ))

(defun iiimcf-server-control-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook
		   (function
		    iiimcf-server-control-exit-from-minibuffer))))

(defun iiimcf-server-control-forward-event (mes)
  (let ((con (iiimp-message-forward-event-contents mes))
	iiimkev emkev kev)
    (iiimp-add-debug-log
     (format "FWD:%S\n" con))
    (if (eq (car con) 'keyevent)
	(progn
	  (setq iiimkev (nth 1 con))
	  (while iiimkev
	    (if (setq kev
		      (iiimcf-translate-iiim-keyevent
		       (car iiimkev)))
		(setq emkev
		      (cons kev emkev)))
	    (setq iiimkev (cdr iiimkev)))
	  (if emkev
	      (iiimcf-server-control-dispatch-emacs-event
	       (nreverse emkev)))))))

;;;
;;; Select input method.
;;;

(defun iiimcf-server-control-completing-read
  (prompt annotated-collection)
  (let ((completion-setup-hook completion-setup-hook))
    (add-hook 'completion-setup-hook
	      (lambda ()
		(let ((inhibit-read-only t)
		      completion-setup-hook)
		  (save-excursion
		    (if (bufferp standard-output)
			(set-buffer standard-output))
		    (erase-buffer)
		    (display-completion-list
		     (mapcar
		      (lambda (x)
			(format "%s (%s)" x (cdr (assoc x annotated-collection))))
		      (all-completions
		       (save-excursion
			 (set-buffer (window-buffer (active-minibuffer-window)))
			 (buffer-string))
		       minibuffer-completion-table
		       minibuffer-completion-predicate
		       )))))))
    (completing-read prompt annotated-collection nil t)))

(defun iiimcf-server-control-maintain-language-list (mes)
  (iiimcf-com-id-put
   (iiimp-message-com-id mes)
   'language-list
   (iiimp-message-im-language-list mes)))

;;; FIXME
(defun iiimcf-server-control-change-language ()
  (interactive)
  (inactivate-input-method)
  (setq iiimcf-server-control-default-language
	(iiimcf-server-control-completing-read
	 "Input Language:"
	 (mapcar
	  (lambda (x)
	    (cons x (iiimcf-get-language-description x)))
	  (iiimcf-com-id-get
	   iiimcf-server-control-com-id
	   'language-list))))
  (activate-input-method 'iiim-server-control))

(defun iiimcf-server-control-maintain-input-method-list (mes)
  (iiimcf-com-id-put
   (iiimp-message-com-id mes)
   'input-method-list
   (nth 1 (assq 'input-method-list
		(iiimp-message-im-attribute-list mes)))))

;;; FIXME
(defun iiimcf-server-control-change-input-method ()
  (interactive)
  (inactivate-input-method)
  (setq iiimcf-server-control-default-input-method
	(completing-read
	 "Input Method:"
	 (mapcar
	  (lambda (x)
	    (cons (nth 2 x) 0))
	  (iiimcf-com-id-get
	   iiimcf-server-control-com-id
	   'input-method-list))
	  nil t))
  (activate-input-method 'iiim-server-control))

(defun iiimcf-server-control-switch-le ()
  (interactive)
  (iiimcf-notify-le-switch iiimcf-server-control-ic-id))

(defun iiimcf-server-control-switch-keyboard ()
  (interactive)
  (let* ((keyboards iiimcf-server-control-keyboard-list)
         (current iiimcf-current-keycode-spec-alist)
         (init-kb (car keyboards)))
    (while keyboards
      (if (eq current (cdar keyboards))
          (setq iiimcf-current-keycode-spec-alist
                (or (cdadr keyboards) (cdr init-kb))
                keyboards nil))
      (setq keyboards (cdr keyboards)))
    (message "keyboard is switched to %s" 
             (car (rassoc iiimcf-current-keycode-spec-alist
                          iiimcf-server-control-keyboard-list)))))

;;;
;;; trigger-notify operations
;;;

(defun iiimcf-server-control-trigger-notify (mes)
  (let ((flag (iiimp-message-trigger-notify-flag mes)))
    (if flag
	nil
      (inactivate-input-method))))

;;;
;;; Preedit operations
;;;

(defun iiimcf-server-control-maintain-preedit-text (mes)
  (let* ((ic-id (iiimp-message-ic-id mes))
	 (uic (iiimcf-ic-id-get ic-id 'iiimcf-server-control-uic))
	 (preedit (iiimcf-get-current-preedit ic-id)))
    (if (cdr preedit)
	(progn
	  (iiimcf-UI-update-preedit-caret-point uic (car preedit))
	  (iiimcf-UI-update-preedit uic (cdr preedit))
	  (iiimcf-UI-toggle-preedit uic t)
	  (iiimcf-UI-enable uic)
	  ;; If we are in preedit, we must not return to the top level.
	  ;; Because other sysmtem that manages the top level like isearch
	  ;; cause confusion by intercepting events.
	  (if (not iiimcf-server-control-own-event-loop)
	      (let ((iiimcf-server-control-own-event-loop t)
		    (iiimcf-server-control-event-loop-uic uic)
		    (keyboard-translate-table
		     (if (eq iiimcf-server-control-keyboard-translate-table t)
			 keyboard-translate-table
		       iiimcf-server-control-keyboard-translate-table)))
		(while (and (cdr (iiimcf-get-current-preedit ic-id))
			    (eq iiimcf-server-control-event-loop-uic
				(iiimcf-UI-current-context)))
		  (iiimcf-event-loop iiimcf-server-control-preedit-state-keymap)))))
      (iiimcf-UI-disable uic)
      (iiimcf-server-control-release-ic ic-id))))

;;;
;;; Lookup-choice operation.
;;;

(defun iiimcf-server-control-draw-lookup-choice (mes)
  (let* ((ic-id (iiimp-message-ic-id mes))
	 (uic (iiimcf-ic-id-get ic-id 'iiimcf-server-control-uic))
	 (lccfg (iiimcf-ic-id-get ic-id 'lookup-choice-config))
	 (lcdd (iiimcf-ic-id-get ic-id 'lookup-choice))
	 (num (aref lccfg 1))
	 (idxf (aref lcdd 0))
	 (idxl (aref lcdd 1))
	 (idx (aref lcdd 2))
	 (cands (aref lcdd 3))
	 (lbs (if (aref lcdd 5) (aref lcdd 4)))
	 (title (aref lcdd 5))
	 (i 0)
	 result)
    (setq cands (nthcdr idxf cands)
	  lbs (nthcdr idxf lbs))
    (if (= num 0) (setq num (- idxl idxf)))
    (while (and (< i num)
		cands)
      (if lbs
	  (setq result (cons (cons (car lbs) (car cands)) result)
		lbs (cdr lbs))
	(setq result (cons (cons (number-to-string i) (car cands))
			   result)))
      (setq cands (cdr cands)
	    i (1+ i)))
    (iiimcf-UI-update-lookup-choice uic (vector (nreverse result)
						idx title))
    (iiimcf-UI-toggle-lookup-choice uic t)))

(defun iiimcf-server-control-clear-lookup-choice (mes)
  (iiimcf-UI-toggle-lookup-choice
   (iiimcf-server-control-message-uic mes) nil))

;;;
;;; draw status operation.
;;;

(defun iiimcf-server-control-draw-status (mes)
  (iiimcf-UI-update-status
   (iiimcf-server-control-message-uic mes)
   (nth 1 (iiimp-message-status-draw-contents mes))))

;;;
;;; draw AUX data operation.
;;;

(defun iiimcf-server-control-draw-aux-data (mes)
  (let ((name (iiimp-message-aux-im-name mes))
	(strs (iiimp-message-aux-string-list mes)))
    (cond ((or (string-match "^jp\\.co\\.justsystem\\..*LookupAux$"
			 name)
	       (string-match "^jp\\.co\\.justsystem\\..*GenericAux$"
			 name))
	   (if strs
	       (let ((i 1)
		     (s (cdr strs))
		     (r (format "%s:" (car strs))))
		 (while s
		   (setq r (concat r (format " %d:%s" i (car s)))
			 i (1+ i)
			 s (cdr s)))
		 (princ r t))
	     ;;(message "")
	     )))))
  
;;;
;;; commit string operation.
;;;

(defun iiimcf-server-control-commit-string (mes)
  (let ((iiimcf-server-control-enable nil))
    (iiimcf-UI-commit-string
     (iiimcf-server-control-message-uic mes)
     (nth 1 (iiimp-message-committed-string mes)))))

;;;
;;; hotkey operation
;;;

(defun iiimcf-server-control-register-hotkeys (mes)
  (iiimcf-im-id-put
   (iiimp-message-im-id mes)
   (intern (format "hotkey-profile-%d"
		   (iiimp-message-register-hotkeys-profie-id mes)))
   (iiimp-message-register-hotkeys-hotkeys mes)))

(defun iiimcf-server-control-select-hotkey-profile (mes)
  (iiimcf-im-id-put
   (iiimp-message-im-id mes)
   'hotkey-profile
   (iiimp-message-select-hotkey-profile-id mes)))

(defun iiimcf-server-control-get-hotkeys-by-label (im-id label)
  (let* ((profile-id
	  (iiimcf-im-id-get im-id 'hotkey-profile))
	 (hotkey-profile (intern (format "hotkey-profile-%d" profile-id)))
	 (hotkeys
	  (iiimcf-im-id-get im-id hotkey-profile))
 	 slot)
    (dolist (hotkey hotkeys)
      (when (equal (nth 4 hotkey) label)
	(setq slot hotkey hotkeys nil)))
    slot))

;;;
;;; event handling routines (invoked with the keymaps).
;;;

;;  Forward events to a IIIM server.
(defun iiimcf-server-control-keyforward (&optional ev)
  (interactive)
  (let* ((uic (iiimcf-server-control-current-uic
	       iiimcf-server-control-im-id))
	 (ic-id (iiimcf-UI-private uic)))
    (unless (equal ic-id iiimcf-server-control-focused-ic-id)
      ;(iiimcf-unset-icfocus iiimcf-server-control-focused-ic-id)
      (setq iiimcf-server-control-focused-ic-id ic-id)
      (iiimcf-set-icfocus ic-id))
    (setq ev 
	  (condition-case err
	      (iiimcf-forward-event ic-id (or ev last-command-event))
	    (iiimp-fatal
	     (iiimcf-server-control-shutdown)
	     (signal (car err) (cdr err)))))
    ;; If the event has not forwarded to the server,
    ;; deal with it by myself.
    (if ev (iiimcf-server-control-dispatch-emacs-event (list ev))))
  nil)

;;;
;;; Register input method.
;;;

(register-input-method
 "iiim-server-control" "Japanese"
 'iiimcf-server-control-activate
 ""  "IIIM server control input method")

(provide 'iiimcf-sc)

;; iiimcf-sc.el ends here.

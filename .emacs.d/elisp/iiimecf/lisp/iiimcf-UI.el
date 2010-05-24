;;; iiimcf-UI.el --- IIIMCF U/I part module.
;;;

;; Copyright (C) 2002 MIYASHITA Hisashi
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
;;; This module manages IM user interface part.

;;; Code:

(require 'iiimcf)

;;; Customizable variables.

(defgroup iiimcf-UI nil
  "*IIIMCF User-Interface."
  :tag "IIIMCF User-Interface"
  :group 'applications
  :group 'i18n)

;; preedit text
(defcustom iiimcf-UI-preedit-open-string "|"
  "*Put this string before preedit string."
  :tag "Preedit open string for IIIMCF User Interface"
  :group 'iiimcf-UI :type 'string)

(defcustom iiimcf-UI-preedit-close-string "|"
  "*Put this string after preedit string."
  :tag "Preedit close string for IIIMCF User Interface"
  :group 'iiimcf-UI :type 'string)

(defface iiimcf-UI-preedit-reverse-face
  '((((class color)
      (background dark))
     (:foreground "black" :background "DarkSlateGray1"))
    (((class color)
      (background light))
     (:foreground "black" :background "DarkSlateGray3"))
    (t (:inverse-video t)))
  "*Face used for preedit reverse attribute"
  :group 'iiimcf-UI)

(defface iiimcf-UI-preedit-underline-face
  '((t (:underline t)))
  "*Face used for preedit underline attribute"
  :group 'iiimcf-UI)

(defface iiimcf-UI-preedit-highlight-face
  '((((class color)
      (background dark))
     (:bold t :foreground "black" :background "SeaGreen1"))
    (((class color)
      (background light))
     (:bold t :foreground "black" :background "SeaGreen3"))
    (t (:bold t)))
  "*Face used for preedit highlight attribute"
  :group 'iiimcf-UI)

(defface iiimcf-UI-preedit-warning-face
  '((t (:bold t :foreground "yellow" :background "red")))
  "*Face used for warning for users"
  :group 'iiimcf-UI)

(defcustom iiimcf-UI-preedit-face-alist
  '((reverse iiimcf-UI-preedit-reverse-face
	     ("[" . "]"))
    (underline iiimcf-UI-preedit-underline-face
	       ("_" . "_"))
    (highlight iiimcf-UI-preedit-highlight-face
	       ("+" . "+"))
    (unknown-ucs iiimcf-UI-preedit-warning-face
		 ("X" . "X")))
  "*A-list used for decorating preedit text."
  :tag "Preedit decoration settings"
  :group 'iiimcf-UI
  :type '(list (list :tag "Used for REVERSE text."
		     (const reverse) face (cons string string))
	       (list :tag "Used for UNDERLINE text."
		     (const underline) face (cons string string))
	       (list :tag "Used for HIGHLIGHT text."
		     (const highlight) face (cons string string))))

(defcustom iiimcf-UI-preedit-use-face-p
  (if window-system t nil)
  "*Use faces when drawing preedit text, if non-nil."
  :tag "Flag whether faces are used for drawing preedit text."
  :group 'iiimcf-UI :type 'boolean)

;; Lookup choice.

(defcustom iiimcf-UI-lookup-choice-style
  'echo-line
  "*Determins the style of lookup-choice display.
'echo-line means lookup-choice is displayed in simple format at minibuffer.
'buffer means lookup-choice is displayed in an isolated buffer."
  :tag "Lookup choice style"
  :group 'iiimcf-UI
  :type '(choice (const echo-line :tag "echo line style")
		 (list :tag "isolated buffer style"
		       (const buffer)
		       (integer :tag "column")
		       (integer :tag "row"))))

;; mode line status

(defcustom iiimcf-UI-input-method-title-format
  "%s"
  "*Format string used to set status information in mode-line."
  :tag "Mode line status format"
  :group 'iiimcf-UI
  :type 'string)

;;; Constants

(defconst iiimcf-UI-lookup-choice-buffer-name "*Lookup choice*")

;;; Variables that hold internal states.

(defvar iiimcf-UI-current-context nil)
(make-variable-buffer-local 'iiimcf-UI-curent-context)
(put 'iiimcf-UI-current-context 'permanent-local t)
(defvar iiimcf-UI-minor-mode-map-flag nil)
(make-variable-buffer-local 'iiimcf-UI-minor-mode-map-flag)
(put 'iiimcf-UI-minor-mode-map-flag 'permanent-local t)

;;;
;;; API
;;;
;;
;; Infomation Get/Set API.
;;
;;   iiimcf-UI-marker
;;   iiimcf-UI-preedit-text
;;   iiimcf-UI-caret-point
;;   iiimcf-UI-preedit-text-flag
;;   iiimcf-UI-candidates
;;   iiimcf-UI-candidates-flag
;;   iiimcf-UI-status
;;   iiimcf-UI-status-flag
;;   iiimcf-UI-keymap
;;   iiimcf-UI-private
;;
;; Operation API.
;;
;;   iiimcf-UI-create-context
;;   iiimcf-UI-destroy-context
;;   iiimcf-UI-update-preedit
;;   iiimcf-UI-toggle-preedit
;;   iiimcf-UI-update-lookup-choice
;;   iiimcf-UI-toggle-lookup-choice
;;   iiimcf-UI-update-status
;;   iiimcf-UI-toggle-status
;;   iiimcf-UI-commit-string
;;   iiimcf-UI-set-keymap
;;   iiimcf-UI-UI-enable
;;   iiimcf-UI-disable


;; UIC structure.
;; [marker
;;  preedit-text
;;  caret-point(relative)
;;  preedit-state
;;  candidates
;;  lookup-choice-state:     'echo-line or  '(<current buffer> col row)
;;  status
;;  status-state
;;  keymap
;;  private]

;; candidates: [ candidate-list index title ]

;;;;;
;; marker
;;   |
;;   V
;;   XXX  <PREEDIT-TEXT>  YYY
;;   preedit-open         preedit-close
;;;;;

(defsubst iiimcf-UI-marker (uic) (aref uic 0))
(defsubst iiimcf-UI-preedit-text (uic) (aref uic 1))
(defsubst iiimcf-UI-caret-point (uic) (aref uic 2))
(defsubst iiimcf-UI-preedit-text-flag (uic) (aref uic 3))
(defsubst iiimcf-UI-candidates (uic) (aref uic 4))
(defsubst iiimcf-UI-lookup-choice-state (uic) (aref uic 5))
(defsubst iiimcf-UI-status (uic) (aref uic 6))
(defsubst iiimcf-UI-status-state (uic) (aref uic 7))
(defsubst iiimcf-UI-keymap (uic) (aref uic 8))
(defsubst iiimcf-UI-private (uic) (aref uic 9))

(defun iiimcf-UI-create-context (&optional buf pt private)
  (let ((mk (make-marker)))
    (set-marker mk
		(or pt (point))
		(or buf (current-buffer)))
    (set-marker-insertion-type mk t)
    (vector mk nil 0 nil nil nil nil nil nil private)))

(defun iiimcf-UI-move-marker (uic &optional buf pt)
  (let ((ops (iiimcf-UI-preedit-text-flag uic))
	(olcs (iiimcf-UI-lookup-choice-state uic))
	(oss (iiimcf-UI-status-state uic))
	(mk (iiimcf-UI-marker uic)))
    (iiimcf-UI-disable uic)
    (iiimcf-UI-toggle-preedit uic nil)
    (iiimcf-UI-toggle-lookup-choice uic nil)
    (iiimcf-UI-toggle-status uic nil)
    (set-marker mk (or pt (point)) (or buf (current-buffer)))
    (if ops (iiimcf-UI-toggle-preedit uic ops))
    (if olcs (iiimcf-UI-toggle-lookup-choice uic olcs))
    (if oss (iiimcf-UI-toggle-status uic oss))))

(defun iiimcf-UI-update-preedit (uic preedit)
  (aset uic 1 preedit)
  (if (iiimcf-UI-preedit-text-flag uic)
      (iiimcf-UI-insert-preedit uic)))

(defun iiimcf-UI-update-preedit-caret-point (uic cp)
  ;; TODO
  (aset uic 2 cp))

(defun iiimcf-UI-toggle-preedit (uic flag)
  (if (and (not flag) (iiimcf-UI-preedit-text-flag uic))
      (let* ((ic-id (iiimcf-UI-private uic)))
	(iiimcf-UI-clear-preedit-text uic))
    (if (and flag (not (iiimcf-UI-preedit-text-flag uic)))
	(let ((ic-id (iiimcf-UI-private uic)))
	  (iiimcf-UI-setup-preedit-text uic)
	  (iiimcf-UI-insert-preedit uic))))
  (aset uic 3 flag))

(defun iiimcf-UI-update-lookup-choice (uic cands)
  (aset uic 4 cands)
  (cond ((eq 'echo-line (iiimcf-UI-lookup-choice-state uic))
	 (iiimcf-UI-draw-lookup-choice-echo-line
	  (iiimcf-UI-marker uic) cands))
	((bufferp (car-safe (iiimcf-UI-lookup-choice-state uic)))
	 (iiimcf-UI-draw-lookup-choice-buffer
	  (car (iiimcf-UI-lookup-choice-state uic)) cands))))

(defun iiimcf-UI-toggle-lookup-choice (uic flag)
  (setq flag (if flag iiimcf-UI-lookup-choice-style))
  (let ((cstate (iiimcf-UI-lookup-choice-state uic)))
    (cond ((eq flag cstate) nil)
	  ((and (bufferp (car-safe cstate))
		(eq 'buffer (car-safe flag))) nil)
	  ((and (not flag)
		(eq 'echo-line cstate))
	   (iiimcf-UI-clear-lookup-choice-echo-line
	    (iiimcf-UI-marker uic))
	   (aset uic 5 nil))
	  ((and (not flag)
		(bufferp (car-safe cstate)))
	   (iiimcf-UI-clear-lookup-choice-buffer
	    (car cstate))
	   (aset uic 5 nil))
	  ((eq 'echo-line flag)
	   (iiimcf-UI-draw-lookup-choice-echo-line
	    (iiimcf-UI-marker uic)
	    (iiimcf-UI-candidates uic))
	   (aset uic 5 'echo-line))
	  ((eq 'buffer (car-safe flag))
	   (let ((buf (generate-new-buffer iiimcf-UI-lookup-choice-buffer-name))
		 (row (nth 1 flag))
		 (col (nth 2 flag)))
	     (aset uic 5 (list buf row col))
	     (iiimcf-UI-draw-lookup-choice-buffer
	      buf (iiimcf-UI-candidates uic))))
	  (t
	   (error "Invalid style specification:%S" flag)))))

(defun iiimcf-UI-update-status (uic stat)
  (aset uic 6 stat)
  (if (iiimcf-UI-status-state uic)
      (iiimcf-UI-draw-status uic)))

(defun iiimcf-UI-toggle-status (uic flag)
  (aset uic 7 flag)
  (if flag
      (iiimcf-UI-draw-status uic)
    (iiimcf-UI-clear-status uic)))


(defmacro iiimcf-UI-buffer-context (x-uic &rest form)
  `(let ((buffer (marker-buffer (iiimcf-UI-marker ,x-uic)))
	 (oldbuf (current-buffer)))
     (if (or (not (buffer-live-p buffer))
	     (eq buffer oldbuf))
	 (progn
	   ,@form)
       (set-buffer buffer)
       ,@form
       (set-buffer oldbuf))))

(defun iiimcf-UI-set-keymap (uic keymap)
  (iiimcf-UI-buffer-context
   uic
   (let ((slot (assq 'iiimcf-UI-minor-mode-map-flag
		     minor-mode-map-alist)))
     (if (and slot
	      (eq (cdr slot) (aref uic 8)))
	 (setcdr slot keymap)))
   (aset uic 8 keymap)))


(defun iiimcf-UI-enable (uic)
  (iiimcf-UI-buffer-context
   uic
   (setq iiimcf-UI-current-context uic
	 iiimcf-UI-minor-mode-map-flag t)
   (setq minor-mode-map-alist
	 (cons (cons 'iiimcf-UI-minor-mode-map-flag
		     (iiimcf-UI-keymap uic))
	       (delq (assq 'iiimcf-UI-minor-mode-map-flag
			   minor-mode-map-alist)
		     minor-mode-map-alist)))))

(defun iiimcf-UI-disable (uic)
  (iiimcf-UI-buffer-context
   uic
   (setq iiimcf-UI-current-context nil
	 iiimcf-UI-minor-mode-map-flag nil)
   (let ((slot (assq 'iiimcf-UI-minor-mode-map-flag
		     minor-mode-map-alist)))
     (if slot
	 (setq minor-mode-map-alist (delq slot minor-mode-map-alist))))))

(defun iiimcf-UI-destroy-context (uic)
  (iiimcf-UI-disable uic)
  (iiimcf-UI-toggle-status uic nil)
  (iiimcf-UI-toggle-preedit uic nil)
  (iiimcf-UI-toggle-lookup-choice uic nil))

(defun iiimcf-UI-dispatch-emacs-event (evs &optional buf save-excursion-p)
  (let ((iiimcf-UI-minor-mode-map-flag nil))
    (iiimcf-dispatch-emacs-event evs buf save-excursion-p)))

;; (defvar iiimcf-server-control-caret-overlay
;;   (make-overlay 0 0))
;; (overlay-put iiimcf-server-control-caret-overlay
;;	     'face 'modeline)

(defvar iiimcf-UI-status "")
(make-variable-buffer-local 'iiimcf-UI-status)

(defun iiimcf-UI-convert-iiim-feedback
  (feedbacks type)
  (if (eq type 'face)
      (let (slot result)
	(while feedbacks
	  (setq slot (assq (car feedbacks)
			   iiimcf-UI-preedit-face-alist)
		feedbacks (cdr feedbacks))
	  (if slot
	      (setq result (cons (nth 1 slot) result))))
	(nreverse result))
    nil))

(defun iiimcf-UI-convert-EIMIL-feedback
  ;;TODO!!!
  (feedbacks type)
  (if (eq type 'face)
      (let (slot result)
	(while feedbacks
	  (setq slot (assq (car feedbacks)
			   iiimcf-UI-preedit-face-alist)
		feedbacks (cdr feedbacks))
	  (if slot
	      (setq result (cons (nth 1 slot) result))))
	(nreverse result))
    nil))

;;;
;;; Preedit operations
;;;

;;
;; XX ... preedit-open-string
;; YY ... preedit-close-string
;;
;;    +---- iiimcf-UI-preedit
;;  +-+------+
;;  v        v
;;  XX abcd YY
;;          ^^-- iiimcf-UI-preedit-end, rear-nonsticky
;;

(defsubst iiimcf-UI-preedit-text-properties (uic)
  (list 'iiimcf-UI-preedit t
	;'intangible t
	'read-only t
	'point-entered 'iiimcf-UI-enter-or-leave-preedit
	'point-left 'iiimcf-UI-enter-or-leave-preedit
	'fontified t
	'iiimcf-UI-context uic))

(defun iiimcf-UI-preedit-text-properties-end (uic)
  (list 'iiimcf-UI-preedit-end t
	;'intangible t
	'rear-nonsticky t
	'start-open t
	'iiimcf-UI-context uic))

(defsubst iiimcf-UI-preedit-enable-p ()
  (get-text-property (point) 'iiimcf-UI-preedit))

(defsubst iiimcf-UI-current-context ()
  (get-text-property (point) 'iiimcf-UI-context))

(defun iiimcf-UI-enter-or-leave-preedit (oldpos newpos)
  ;; TODO!!!
  nil)

(defun iiimcf-UI-put-face-to-preedit (str)
  (let ((len (length str))
	(pts 0)
	pte cprop)
    (setq str (copy-sequence str))
    (while pts
      (setq cprop (get-text-property pts 'iiim-feedback str)
	    pte (next-single-property-change
		 pts 'iiim-feedback str))
      (if cprop
	  (put-text-property
	   pts (or pte len)
	   'face (iiimcf-UI-convert-iiim-feedback
		  cprop 'face)
	   str))
      (setq pts pte))
    str))

(defun iiimcf-UI-enclose-preedit-with-string (str caret)
  (let ((result "")
	(pts 0)
	(caretdiff 0)
	pte cprop cprop2 pprop
	elem slot hstr tstr)
    (while pts
      (setq cprop (get-text-property pts 'iiim-feedback str)
	    cprop2 cprop
	    pte (next-single-property-change
		 pts 'iiim-feedback str))
      (setq hstr "")
      (while (setq elem (car cprop))
	(if (memq elem pprop)
	    (setq pprop (delq elem pprop))
	  (if (setq slot (assq elem
			       iiimcf-UI-preedit-face-alist))
	      (setq hstr (concat hstr (car (nth 2 slot))))))
	(setq cprop (cdr cprop)))
      (setq tstr "")
      (while pprop
	(if (setq slot (assq (car pprop)
			     iiimcf-UI-preedit-face-alist))
	    (setq tstr (concat tstr (cdr (nth 2 slot)))))
	(setq pprop (cdr pprop)))
      (setq result (concat result tstr hstr (substring str pts pte)))
      (if (> caret pts) (setq caretdiff
			      (+ caretdiff
				 (length tstr)
				 (length hstr))))
      (setq pts pte
	    pprop cprop2))
    (while pprop
      (if (setq slot (assq (car pprop)
			   iiimcf-UI-preedit-face-alist))
	  (setq result (concat result (cdr (nth 2 slot)))))
      (setq pprop (cdr pprop)))
    (cons result (+ caret caretdiff))))

(defun iiimcf-UI-format-preedit (str caret)
  (if iiimcf-UI-preedit-use-face-p
      (cons (iiimcf-UI-put-face-to-preedit str)
	    caret)
    (iiimcf-UI-enclose-preedit-with-string
     str caret)))

(defun iiimcf-UI-setup-preedit-text (uic)
  (save-excursion
    (let ((buffer-undo-list t)
	  (marker (iiimcf-UI-marker uic))
	  pss pse)
      (set-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (setq pss (point))
      (insert iiimcf-UI-preedit-open-string)
      (setq pse (point))
      (insert iiimcf-UI-preedit-close-string)
      (add-text-properties
       pse (point)
       (iiimcf-UI-preedit-text-properties-end uic))
      (add-text-properties
       pss (point)
       (iiimcf-UI-preedit-text-properties uic))
      (set-marker marker pss)
      marker)))

(defun iiimcf-UI-insert-preedit (uic)
  (let* ((marker (iiimcf-UI-marker uic))
	 (text (iiimcf-UI-preedit-text uic))
	 (caret (iiimcf-UI-caret-point uic))
	 ;; disable the font-lock function invoked by
	 ;; after-change-functions during preedit.
	 ;; Even read-only text-property cannot protect inserted preedit
	 ;; text from modifications of the face text-property by font-lock.
	 (font-lock-fontify-region-function (function ignore))	 
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (oldbuf (current-buffer))
	 (pts (marker-position marker))
	 showtext pte)
    (set-buffer (marker-buffer marker))
    (setq pte
	  (next-single-property-change
	   pts 'iiimcf-UI-preedit-end))
    (if (null pte)
	(error "PREEDIT TEXT IS BROKEN!!"))
    ;; format
    (setq caret (iiimcf-UI-format-preedit text caret))
    (setq showtext (car caret)
	  caret (cdr caret))
    (add-text-properties
     0 (length showtext)
     (iiimcf-UI-preedit-text-properties uic)
     showtext)
    ;; insert
    (setq pts (+ pts (length iiimcf-UI-preedit-open-string)))
    (let ((buffer-undo-list t)
	  ;; jit-lock force to change 'fontified text property.
	  (jit-lock-mode nil))
      (delete-region pts pte)
      (goto-char pts)
      (insert showtext)
      (goto-char (+ pts caret))
      ;; Don't let jit-lock refontify preedit!!
      (if (and (boundp 'jit-lock-first-unfontify-pos)
	       jit-lock-first-unfontify-pos)
	  (setq jit-lock-first-unfontify-pos
		(+ pte (length iiimcf-UI-preedit-close-string)))))
    (set-buffer oldbuf)))

(defun iiimcf-UI-show-preedit-minibuffer-p ()
  (and input-method-use-echo-area
       (iiimcf-UI-preedit-enable-p)))

(defun iiimcf-UI-show-preedit-in-minibuffer (uic)
  (let (pt mbuf)
    (save-excursion
      ;; clear echo area.
      (message nil)
      (setq mbuf (window-buffer (minibuffer-window)))
      (set-buffer mbuf)
      (erase-buffer)
      (insert "Preedit:")
      (setq pt (point))
      (insert (iiimcf-UI-preedit-text uic) " ")
      ; (move-overlay
      ;  iiimcf-UI-caret-overlay
      ;  (+ pt caret) (+ 1 pt caret) mbuf)
      )))

(defun iiimcf-UI-clear-preedit-text (uic)
  (if (iiimcf-UI-show-preedit-minibuffer-p)
      ;; erase minibuffer.
      (save-excursion
	(let ((mbuf (window-buffer (minibuffer-window))))
	  (set-buffer mbuf)
	  (erase-buffer)
	  ;; (move-overlay
	  ;; iiimcf-server-control-caret-overlay
	  ;; 0 0 mbuf)
	  ))
    ;; Erase inserted preedit.
    (let* ((marker (iiimcf-UI-marker uic))
	   (pts (if marker (marker-position marker)))
	   (inhibit-read-only t)
	   pte)
      (if pts
	  (progn
	    (save-excursion
	      (set-buffer (marker-buffer marker))
	      (setq pte
		    (next-single-property-change
		     pts 'iiimcf-UI-preedit-end))
	      (if (null pte)
		  (error "PREEDIT TEXT IS BROKEN!!"))
	      (let ((buffer-undo-list t))
		(delete-region
		 pts (+ pte
			(length
			 iiimcf-UI-preedit-close-string)))))
	    (goto-char pts))))))

;;;
;;; Lookup-choice operation.
;;;

(defun iiimcf-UI-draw-lookup-choice-echo-line (mk candidates)
   (let (buf w f mw lb str linestr nlinestr
	 (cands (aref candidates 0))
	 (index (aref candidates 1))
	 (title (aref candidates 2)))
     (setq buf (marker-buffer mk)
	   w (get-buffer-window buf t)
	   f (if w (window-frame w) (selected-frame))
	   mw (window-width (minibuffer-window f))
	   linestr (concat title ":: "))
     (while (and cands
		 (progn
		   (setq lb (car (car cands))
			 str (cdr (car cands))
			 nlinestr 
			 (if (= 0 index) (concat linestr (format "%s:<%s> " lb str))
			   (concat linestr (format "%s:%s " lb str))))
		   (< (length nlinestr) mw)))
       (setq linestr nlinestr
	     index (1- index)
	     cands (cdr cands)))
     (message linestr)
     cands))

(defun iiimcf-UI-clear-lookup-choice-echo-line (marker)
  (message nil))

(defun iiimcf-UI-draw-lookup-choice-buffer (buf candidates)
  (with-output-to-temp-buffer
      (buffer-name buf)
    (save-excursion
      (set-buffer standard-output)
      (let ((cands (aref candidates 0))
	    (index (aref candidates 1))
	    (title (aref candidates 2)))
	(insert (format "Title:%s\n" title))
	(while cands
	  (if (= 0 index)
	      (insert (format "%c:<%s>\n"
			      (car (car cands))
			      (cdr (car cands))))
	    (insert (format "%c:%s\n"
			    (car (car cands))
			    (cdr (car cands)))))
	  (setq cands (cdr cands)
		index (1- index)))
	cands))))

(defun iiimcf-UI-clear-lookup-choice-buffer (buf)
  (kill-buffer buf))

(defun iiimcf-UI-draw-lookup-choice (uic)
  (cond ((eq (iiimcf-UI-lookup-choice-state uic) 'minibuf)
	 (iiimcf-UI-draw-lookup-choice-echo-line
	  (iiimcf-UI-marker uic)
	  (iiimcf-UI-candidates uic)))
	((bufferp (iiimcf-UI-lookup-choice-state uic))
	 (iiimcf-UI-draw-lookup-choice-buffer
	  (iiimcf-UI-lookup-choice-state uic)
	  (iiimcf-UI-candidates uic)))))

(defun iiimcf-UI-clear-lookup-choice (uic)
  (let* ((buf (get-buffer "*Lookup choice*"))
	 (win (get-buffer-window buf)))
    (if win (delete-window win))))

;;;
;;; draw status operation.
;;;

(defun iiimcf-UI-draw-status (uic)
  (let* ((marker (iiimcf-UI-marker uic))
	 (buf (marker-buffer marker))
	 (oldbuf (current-buffer)))
    (if (and buf (not (eq buf oldbuf))) (set-buffer buf))
    (setq iiimcf-UI-status
	  (iiimcf-UI-status uic))
    (setq current-input-method-title
	  (format iiimcf-UI-input-method-title-format iiimcf-UI-status))
    (force-mode-line-update)
    (if (not (eq buf oldbuf)) (set-buffer oldbuf))))

(defun iiimcf-UI-clear-status (uic)
  (let* ((marker (iiimcf-UI-marker uic))
	 (buf (marker-buffer marker))
	 (oldbuf (current-buffer)))
    (if (and buf (not (eq buf oldbuf))) (set-buffer buf))
    (setq iiimcf-UI-status ""
	  current-input-method-title "")
    (force-mode-line-update)
    (if (not (eq buf oldbuf)) (set-buffer oldbuf))))

;;;
;;; commit string operation.
;;;

(defun iiimcf-UI-commit-string (uic str)
  (let* ((marker (iiimcf-UI-marker uic))
	 (oldbuf (current-buffer))
	 (buf (marker-buffer marker))
	 (pt (marker-position marker))
	 (inhibit-point-motion-hooks t)
	 generated-events)
    ;; strip text properties from str.
    (setq str (copy-sequence str))
    (set-text-properties 0 (length str) nil str)
    (if (null pt) (error "UIC position is not located!!"))
    (set-buffer buf)
    (goto-char pt)
    (setq generated-events (iiimcf-emacs-string-to-events str))
    ;; dispatch them immediately!
    (iiimcf-UI-dispatch-emacs-event generated-events nil nil)
    ;; (insert str)
    (set-buffer oldbuf)
    (run-hooks 'input-method-after-insert-chunk-hook)))

;;;

(provide 'iiimcf-UI)

;; iiimcf-UI.el ends here.

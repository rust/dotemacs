;;; iiimcf.el --- IIIM Client Framework Support on Emacs
;;;

;; Copyright (C) 2000-2002 MIYASHITA Hisashi
;; Copyright (C) 2004-2005 Hiroshi Miura, NTT DATA Intellilink Co. 

;; Author: MIYASHITA Hisashi <himi@m17n.org>
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

;; This module supplies client framework on Emacs.
;; It consists of (1) higher level event manager and dispatcher;
;; (2) minimum default event handlers mainly for default responce
;; to the server(s); (3) keyevent translater; (4) IIIM input-context
;; state absorption layer ;and so on.
;; This module use iiimp.el for low level protocol communication
;; with IIIM server sides.

;;; Code:

(eval-and-compile (require 'cl))
(require 'iiimp)

;;; version.

(defconst iiimcf-version "0.7 (Omine)")

;;; error handling.

(defun iiimcf-prevent-error (&rest args)
  nil)

(defun iiimcf-error (&rest args)
  (signal 'iiimcf-error (apply 'format args)))
(put 'iiimcf-error 'error-conditions '(iiimcf-error error))

;;; IIIM language information alist.

;; (<ISO 639 2-letter code + variation code(optional)>
;;   EXPLANATION OPTION-ALIST)
;; EXPLANATION := ((<language-name> <description>) ...)
;; OPTION-ALIST := To be determined.
(defvar iiimcf-language-alist
  '(("aa" (("English" "Afar")))
    ("ab" (("English" "Abkhazian")))
    ("af" (("English" "Afrikaans")))
    ("am" (("English" "Amharic")))
    ("ar" (("English" "Arabic")
	   ("Japanese" "アラビア語")))
    ("as" (("English" "Assamese")))
    ("ay" (("English" "Aymara")))
    ("az" (("English" "Azerbaijani")))
    ("ba" (("English" "Bashkir")))
    ("be" (("English" "Byelorussian")))
    ("bg" (("English" "Bulgarian")))
    ("bh" (("English" "Bihari")))
    ("bi" (("English" "Bislama")))
    ("bn" (("English" "Bengali" "Bangla")))
    ("bo" (("English" "Tibetan")
	   ("Japanese" "チベット語")))
    ("br" (("English" "Breton")))
    ("ca" (("English" "Catalan")))
    ("co" (("English" "Corsican")))
    ("cs" (("English" "Czech")))
    ("cy" (("English" "Welsh")))
    ("da" (("English" "Danish")))
    ("de" (("English" "German")
	   ("Japanese" "ドイツ語")))
    ("dz" (("English" "Bhutani")))
    ("el" (("English" "Greek")
	   ("Japanese" "ギリシャ語")))
    ("en" (("English" "English" "American")
	   ("Japanese" "英語")))
    ("eo" (("English" "Esperanto")
	   ("Japanese" "エスペラント語")))
    ("es" (("English" "Spanish")
	   ("Japanese" "スペイン語")))
    ("et" (("English" "Estonian")))
    ("eu" (("English" "Basque")))
    ("fa" (("English" "Persian")))
    ("fi" (("English" "Finnish")
	   ("Japanese" "フィンランド語")))
    ("fj" (("English" "Fiji")))
    ("fo" (("English" "Faeroese")))
    ("fr" (("English" "French")
	   ("Japanese" "フランス語")))
    ("fy" (("English" "Frisian")))
    ("ga" (("English" "Irish")))
    ("gd" (("English" "Gaelic" "Scots Gaelic")))
    ("gl" (("English" "Galician")))
    ("gn" (("English" "Guarani")))
    ("gu" (("English" "Gujarati")))
    ("ha" (("English" "Hausa")))
    ("hi" (("English" "Hindi")
	   ("Japanese" "ヒンズー語")))
    ("hr" (("English" "Croatian")))
    ("hu" (("English" "Hungarian")))
    ("hy" (("English" "Armenian")))
    ("ia" (("English" "Interlingua")))
    ("ie" (("English" "Interlingue")))
    ("ik" (("English" "Inupiak")))
    ("in" (("English" "Indonesian")))
    ("is" (("English" "Icelandic")))
    ("it" (("English" "Italian")
	   ("Japanese" "イタリア語")))
    ("iw" (("English" "Hebrew")
	   ("Japanese" "ヘブライ語")))
    ("ja" (("English" "Japanese")
	   ("Japanese" "日本語")))
    ("ji" (("English" "Yiddish")))
    ("jw" (("English" "Javanese")))
    ("ka" (("English" "Georgian")))
    ("kk" (("English" "Kazakh")))
    ("kl" (("English" "Greenlandic")))
    ("km" (("English" "Cambodian")))
    ("kn" (("English" "Kannada")))
    ("ko" (("English" "Korean")
	   ("Japanese" "朝鮮語")))
    ("ks" (("English" "Kashmiri")))
    ("ku" (("English" "Kurdish")))
    ("ky" (("English" "Kirghiz")))
    ("la" (("English" "Latin")))
    ("ln" (("English" "Lingala")))
    ("lo" (("English" "Laothian")))
    ("lt" (("English" "Lithuanian")))
    ("lv" (("English" "Latvian" "Lettish")))
    ("mg" (("English" "Malagasy")))
    ("mi" (("English" "Maori")))
    ("mk" (("English" "Macedonian")))
    ("ml" (("English" "Malayalam")))
    ("mn" (("English" "Mongolian")))
    ("mo" (("English" "Moldavian")))
    ("mr" (("English" "Marathi")))
    ("ms" (("English" "Malay")))
    ("mt" (("English" "Maltese")))
    ("my" (("English" "Burmese")))
    ("na" (("English" "Nauru")))
    ("ne" (("English" "Nepali")))
    ("nl" (("English" "Dutch")
	   ("Japanese" "オランダ語")))
    ("no" (("English" "Norwegian")))
    ("oc" (("English" "Occitan")))
    ("om" (("English" "Oromo" "Afan")))
    ("or" (("English" "Oriya")))
    ("pa" (("English" "Punjabi")))
    ("pl" (("English" "Polish")))
    ("ps" (("English" "Pashto" "Pushto")))
    ("pt" (("English" "Portuguese")))
    ("qu" (("English" "Quechua")))
    ("rm" (("English" "Rhaeto-Romance")))
    ("rn" (("English" "Kirundi")))
    ("ro" (("English" "Romanian")))
    ("ru" (("English" "Russian")
	   ("Japanese" "ロシア語")))
    ("rw" (("English" "Kinyarwanda")))
    ("sa" (("English" "Sanskrit")))
    ("sd" (("English" "Sindhi")))
    ("sg" (("English" "Sangro")))
    ("sh" (("English" "Serbo-Croatian")))
    ("si" (("English" "Singhalese")))
    ("sk" (("English" "Slovak")))
    ("sl" (("English" "Slovenian")))
    ("sm" (("English" "Samoan")))
    ("sn" (("English" "Shona")))
    ("so" (("English" "Somali")))
    ("sq" (("English" "Albanian")))
    ("sr" (("English" "Serbian")))
    ("ss" (("English" "Siswati")))
    ("st" (("English" "Sesotho")))
    ("su" (("English" "Sudanese")))
    ("sv" (("English" "Swedish")))
    ("sw" (("English" "Swahili")))
    ("ta" (("English" "Tamil")))
    ("te" (("English" "Tegulu")))
    ("tg" (("English" "Tajik")))
    ("th" (("English" "Thai")
	   ("Japanese" "タイ語")))
    ("ti" (("English" "Tigrinya")))
    ("tk" (("English" "Turkmen")))
    ("tl" (("English" "Tagalog")))
    ("tn" (("English" "Setswana")))
    ("to" (("English" "Tonga")))
    ("tr" (("English" "Turkish")))
    ("ts" (("English" "Tsonga")))
    ("tt" (("English" "Tatar")))
    ("tw" (("English" "Twi")))
    ("uk" (("English" "Ukrainian")))
    ("ur" (("English" "Urdu")))
    ("uz" (("English" "Uzbek")))
    ("vi" (("English" "Vietnamese")))
    ("vo" (("English" "Volapuk")))
    ("wo" (("English" "Wolof")))
    ("xh" (("English" "Xhosa")))
    ("yo" (("English" "Yoruba")))
    ("zh" (("English" "Chinese")
	   ("Japanese" "中国語")))
    ("zh_CN" (("English" "Chinese(Simplified)")
	      ("Japanese" "中国語(簡体字)")))
    ("zh_TW" (("English" "Chinese(Traditional)")
	      ("Japanese" "中国語(繁体字)")))
    ("zu" (("English" "Zulu")))))

(defun iiimcf-get-language-description (lang)
  (let ((slot (nth 1 (assoc lang iiimcf-language-alist)))
	cand)
    (if current-language-environment
	(setq cand (assoc current-language-environment
			  slot)))
    (if (null cand)
	(setq cand (car slot)))
    (nth 1 cand)))

;;; Emacs event and IIIM keyevent translator.

;; Because IIIMF keyevent is based on Java environment, IIIMECF tries
;; to emulate Java KeyEvent data format.  But, owing to the various
;; problems, this emulation is maybe incomplete.
;;   The KeyEvent class of Java maintains keycode, keychar, and
;; modifiers.  Among these, the meanings of keychar seem to be clear,
;; i.e., it is a character code generated by the event.  The meanings
;; of keycode is, however, ambiguous because it holds a virtual key
;; code that specifies a certain key. Eventually we cannot help assuming
;; a generalized keyboard.  For example, VK_A virtual key code itself does
;; not specify `A' at all.  it specifies a certain key, which probably
;; says `A' on the key top :-), but a Japanese keyboard driver may translate
;; it to a Japanese hiragana character: `chi'.
;;   In Emacs environment, (event-basic-type) has a weak
;; correspondence to the above virtual key code.  However,
;; there is a serious incompatibility between these.  
;; In Java environment, virtual key codes from VK_A to VK_Z are
;; mapped into numbers from 65(`A' in ASCII) to 90(`Z' in ASCII).
;; In Emacs environment, numbers of (event-basic-type) generated
;; from the corresponding key events range from 97(`a' in ASCII)
;; to 122('z' in ASCII).

(defun iiimcf-numseq (from to)
  (let (seq)
    (while (<= from to)
      (setq seq (cons from seq)
            from (1+ from)))
    (nreverse seq)))

(defconst iiimcf-event-spec-alist
  '((component-event . 1)
    (container-event . 2)
    (focus-event . 4)
    (key-event . 8)
    (mouse-event . 16)
    (mouse-motion-event . 32)
    (window-event . 64)
    (action-event . 128)
    (adjustment-event . 256)
    (item-event . 512)
    (text-event . 1024)))

(defconst iiimcf-modifier-spec-alist
  '((shift . 1)
    (control . 2)
    (meta . 4)
    (alt . 8)
    (button-1 . 16)
    (button-2 . 8)
    (button-3 . 4)))

;; emacs event to keycode. 
;; (event keycode [keychar [keymod]])
;;   keychar is nil -> emacs event or 0 if event is symbol.

(defvar iiimcf-keycode-spec-alist
  `((13 10 0)
    (32 32)
    ,@(mapcar #'(lambda (x) (list x x 0))
	      (iiimcf-numseq 1 31))
    (return 10)
    (backspace 8)
    (tab 9)
    (cancel 3 65535)
    (clear 12 65535)
    (pause 19)
    (capslock 20)
    (escape 27)
    (prior 33 65535)
    (next 34 65535)
    (end 35 65535)
    (home 36 65535)
    (left 37 65535)
    (up 38 65535)
    (right 39 65535)
    (down 40 65535)
    (f1 112 65535)
    (f2 113 65535)
    (f3 114 65535)
    (f4 115 65535)
    (f5 116 65535)
    (f6 117 65535)
    (f7 118 65535)
    (f8 119 65535)
    (f9 120 65535)
    (f10 121 65535)
    (f11 122 65535)
    (f12 123 65535)
    (print 154)
    (delete 127)
    (numlock 144 65535)
    (scroll 145 65535)
    (insert 155 65535)
    (help 156 65535)
    (convert 28 65535)
    (nonconvert 29 65535)
    (accept 30 65535)
    (modechange 31 65535)
    (kana 21 65535)
    (kanji 25 65535)
    (zenkaku-hankaku 25 65535)
    (undefined 0)
    (char-undefined 0)))

(defvar iiimcf-ascii-keycode-spec-alist
  `(
    (?! ?\x0205)
    (?\" ?\x0099)
    (?# ?\x0208)
    (?$ ?\x0203)
    (?% ?5 nil 1)
    (?& ?\x96)
    (?' ?\xde)
    (?\( ?\x0207)
    (?\) ?\x020a)
    (?* ?\x97)
    (?+ ?\x0209)
    ;; 0x2c - 0x39: ascii == vk
    ,@(mapcar #'(lambda (x) (list x x))
	      (iiimcf-numseq ?\x2c ?\x39))
    (?: ?\x0201)
    (?\; ?\;)
    (?< ?\x99)
    (?= ?=)
    (?> ?\xa0)
    (?? ?/ nil 1)
    (?@ ?\x0200)
    ;; 0x41-0x5a ascii=vk+shift
    ,@(mapcar #'(lambda (x) (list x x nil 1))
	      (iiimcf-numseq ?\x41 ?\x5a))
    (?\[ ?\[)
    (?\\ ?\\)
    (?\] ?\])
    (?^ ?\x0202)
    (?_ ?\x020b)
    (?` ?\xc0)
    ;; 0x61-0x7a ascii=vk+32.
    ,@(mapcar #'(lambda (x) (list x (- x 32)))
	      (iiimcf-numseq ?\x61 ?\x7a))
    (?{ ?\xa1)
    (?| ?\\ nil 1)
    (?} ?\xa2)
    (?~ ?` nil 1)))

(defvar iiimcf-kana-keycode-spec-alist
  '((?1 0 ?\xff87) ; kana-NU
    (?2 0 ?\xff8c) ; kana-HU
    (?3 0 ?\xff71) ; kana-A
    (?4 0 ?\xff73) ; kana-U
    (?5 0 ?\xff74) ; kana-E
    (?6 0 ?\xff75) ; kana-O
    (?7 0 ?\xff94) ; kana-YA
    (?8 0 ?\xff95) ; kana-YU
    (?9 0 ?\xff96) ; kana-YO
    (?0 0 ?\xff9c) ; kana-WA

    (?- 0 ?\xff8e) ; kana-HO
    (?^ 0 ?\xff8d) ; kana-HE
    (?\\ 0 ?\xff70) ; prolongedsound

    (?q 0 ?\xff80) ; kana-TA
    (?w 0 ?\xff83) ; kana-TE
    (?e 0 ?\xff72) ; kana-I
    (?r 0 ?\xff7d) ; kana-SU
    (?t 0 ?\xff76) ; kana-KA
    (?y 0 ?\xff9d) ; kana-N
    (?u 0 ?\xff85) ; kana-NA
    (?i 0 ?\xff86) ; kana-NI
    (?o 0 ?\xff97) ; kana-RA
    (?p 0 ?\xff7e) ; kana-SE
    (?@ 0 ?\xff9e) ; voicedsound
    (?\[ 0 ?\xff9f) ; semivoicedsound

    (?a 0 ?\xff81) ; kana-TI
    (?s 0 ?\xff84) ; kana-TO
    (?d 0 ?\xff7c) ; kana-SI
    (?f 0 ?\xff8a) ; kana-HA
    (?g 0 ?\xff77) ; kana-KI
    (?h 0 ?\xff78) ; kana-KU
    (?j 0 ?\xff8f) ; kana-MA
    (?k 0 ?\xff89) ; kana-NO
    (?l 0 ?\xff98) ; kana-RI
    (?\; 0 ?\xff9a) ; kana-RE
    (?: 0 ?\xff79) ; kana-KE
    (?\] 0 ?\xff91) ; kana-MU

    (?z 0 ?\xff82) ; kana-TU
    (?x 0 ?\xff7b) ; kana-SA
    (?c 0 ?\xff7f) ; kana-SO
    (?v 0 ?\xff8b) ; kana-HI
    (?b 0 ?\xff7a) ; kana-KO
    (?n 0 ?\xff90) ; kana-MI
    (?m 0 ?\xff93) ; kana-MO
    (?, 0 ?\xff88) ; kana-NE
    (?. 0 ?\xff99) ; kana-RU
    (?/ 0 ?\xff92) ; kana-ME
    (?_ 0 ?\xff9b) ; kana-RO

    (?# 0 ?\xff67) ; kana-a
    (?$ 0 ?\xff69) ; kana-u
    (?% 0 ?\xff6a) ; kana-e
    (?& 0 ?\xff6b) ; kana-o
    (?' 0 ?\xff6c) ; kana-ya
    (?\( 0 ?\xff6d) ; kana-yu
    (?\) 0 ?\xff6e) ; kana-yo
    (?~ 0 ?\xff66) ; kana-WO
    (?E 0 ?\xff68) ; kana-i
    (?{ 0 ?\xff62) ; kana-openingbracket
    (?} 0 ?\xff63) ; kana-closingbracket
    (?Z 0 ?\xff6f) ; kana-tu
    (?< 0 ?\xff64) ; kana-comma
    (?> 0 ?\xff61) ; kana-fullstop
    (?? 0 ?\xff65))) ; kana-middledot

(defvar iiimcf-current-keycode-spec-alist
  'iiimcf-ascii-keycode-spec-alist)
  ;'iiimcf-kana-keycode-spec-alist)

(defconst iiimcf-keycode-hash-obarray
  (let ((sobarray (make-vector 203 nil))
	(sl iiimcf-keycode-spec-alist)
	el sym)
    (dolist (el sl)
      (setq sym
	    (intern (concat "c:" (number-to-string (nth 1 el)))
		    sobarray))
      (set sym el))
    sobarray)
  "This holds the hash table from IIIM keycode to keycode spec.")

(defun iiimcf-emacs-base-event-pre-translate (base)
  (or (assq base iiimcf-keycode-spec-alist)
      (assq base (eval iiimcf-current-keycode-spec-alist))))

(defun iiimcf-iiim-keycode-pre-translate (kcode)
  (symbol-value
   (intern-soft (concat "c:" (number-to-string kcode))
		iiimcf-keycode-hash-obarray)))

(defun iiimcf-translate-iiim-keymodifier (kmod)
  (let ((mod (logior (lsh (car kmod) 16)
		     (cdr kmod)))
	(cand iiimcf-modifier-spec-alist)
	result)
    (while cand
      (if (/= 0 (logand mod (cdr (car cand))))
	  (setq result (cons (car (car cand)) result)))
      (setq cand (cdr cand)))
    result))

;; Emacs-XEmacs imcompatibility absorption layer...
(defsubst iiimcf-event-to-char (event)
  (cond ((fboundp (function event-to-character))
	 (setq event (event-to-character event))
	 (if event (char-to-ucs event)))
	((and (numberp event)
	      ;; Practically, normal events don't propagate
	      ;; non-ASCII characters.
	      (>= event 32)
	      (or (< event 127)
		  (> event 159)))
	 event)
	(t nil)))

;; Emacs-XEmacs imcompatibility absorption layer...
(defsubst iiimcf-emacs-string-to-events (str)
  (cond ((and (fboundp (function string-make-unibyte))
	      (boundp 'enable-multibyte-characters))
	 (string-to-list
	  (if enable-multibyte-characters
	      str
	    (string-make-unibyte str))))
	((fboundp (function character-to-event))
	 (mapcar (function character-to-event)
		 (string-to-list str)))
	(t
	 (string-to-list str))))

;; Emacs-XEmacs imcompatibility absorption layer...
(defsubst iiimcf-char-to-event (ucs)
  (cond ((fboundp (function character-to-event))
	 (character-to-event (ucs-to-char ucs)))
	(t
	 (ucs-to-char ucs))))

;; Emacs-XEmacs imcompatibility absorption layer...
(defsubst iiimcf-event-basic-type (event)
  (cond ((fboundp (function event-basic-type))
	 (event-basic-type event))
	((fboundp (function event-key))
	 (event-key event))
	(t
	 (error "Cannot find any equivalents of event-basic-type."))))

(defun iiimcf-translate-emacs-event (event)
  "Translate Emacs event to IIIM event if possible.
If impossible, return nil."
  (let* ((mods (event-modifiers event))
	 (base (or (iiimcf-event-basic-type event) event))
	 kcode
	 (kchar (or (iiimcf-event-to-char event) 0))
	 (kmod 0)
	 (ts (current-time))
	 (slot (iiimcf-emacs-base-event-pre-translate event)))

    (unless slot
      (while mods
	(setq kmod (logior
		    kmod
		    (cdr (assq
			  (car mods)
			  iiimcf-modifier-spec-alist)))
	      mods (cdr mods)))
      (setq slot (iiimcf-emacs-base-event-pre-translate base)))

    (if slot
	(setq kcode (nth 1 slot)
	      kchar (or (nth 2 slot) (if (symbolp event) 0 event))
	      kmod  (logior kmod
			    (or (nth 3 slot) 0))))
    (if (and (null kcode) (numberp kchar)) (setq kcode 0))
    (if (numberp kcode)
	(list (cons 0 kcode)
	      (cons 0 kchar)
	      (cons 0 kmod)
	      (cons (nth 1 ts) (nth 2 ts))))))

(defun iiimcf-translate-iiim-keyevent (keyevent)
  "Translate IIIM event to Emacs event if possible.
If impossible, return nil."
  (let* ((kcode (car keyevent))
	 (kchar (nth 1 keyevent))
	 (kmod (nth 2 keyevent))
	 (mod (logior (lsh (car kmod) 16)
		      (cdr kmod)))
	 (char (logior (lsh (car kchar) 16)
		       (cdr kchar)))
	 key)
    (cond ((/= (car kcode) 0) nil)
	  ((setq key (iiimcf-iiim-keycode-pre-translate
		      (cdr kcode)))
	   (event-convert-list
	    (nconc (iiimcf-translate-iiim-keymodifier kmod)
		   (list (car key)))))
	  ((= mod 0)
	   (iiimcf-char-to-event char))
	  ((/= mod 0)
	   (event-convert-list
	    (nconc (iiimcf-translate-iiim-keymodifier kmod)
		   (list (cdr kcode)))))
	  (t (cdr kcode)))))

;;;
;;; Emacs Event management
;;;

(defun iiimcf-event-loop (keymap)
    (if (and (fboundp (function dispatch-event))
	     (fboundp (function next-event)))
	;; XEmacs
	(let ((overriding-local-map keymap)
	      (input-method-function nil))
	  (dispatch-event (next-event)))
      ;; Emacs
      (let (keyseq cmd)
	(let ((overriding-local-map keymap)
	      (input-method-function nil))
	  (setq keyseq (read-key-sequence
			nil nil nil t)
		cmd (key-binding keyseq t)))
	(if (commandp cmd)
	    (progn
	      (setq last-command-event (aref keyseq
					     (1- (length keyseq)))
		    last-command this-command
		    this-command cmd)
	      (call-interactively cmd))))))

;; overriding-local-map save/restore functions
(defun iiimcf-set-overriding-local-map
  (val save &optional buffer-local-p)
  (if buffer-local-p
      (progn
	(set save (default-value 'overriding-local-map))
	(make-local-variable 'overriding-local-map)
	(put 'overriding-local-map 'permanent-local t))
    (set save overriding-local-map))
  (setq overriding-local-map val))

(defun iiimcf-reset-overriding-local-map (save)
  (setq overriding-local-map (symbol-value save)))

;; Emacs-XEmacs imcompatibility absorption layer...
(defun iiimcf-dispatch-emacs-event-internal ()
  (let ((input-method-function nil)
	keyseq cmd)
    (if (and (fboundp (function dispatch-event))
	     (fboundp (function next-event)))
	;; XEmacs
	(let ((new-overriding-local-map overriding-local-map))
	  (while unread-command-events
	    (let ((overriding-local-map nil))
	      (dispatch-event (next-event))
	      (if overriding-local-map
		  (setq new-overriding-local-map
			overriding-local-map))))
	  (setq overriding-local-map
		new-overriding-local-map))
      ;; Emacs.
      (while unread-command-events
	(let ((overriding-local-map nil))
	  (setq keyseq (read-key-sequence nil nil nil)
		cmd (key-binding keyseq t)))
	(if (and (commandp cmd)
		 (not (eq cmd 'iiimcf-server-control-keyforward)))
	    (progn
	      (setq last-command-event (aref keyseq
					     (1- (length keyseq)))
		    last-command this-command
		    this-command cmd)
	      ;; sometimes current-prefix-arg became non-nil for unknown reason.
	      (let ((current-prefix-arg nil)) 
		(call-interactively cmd)))))))
  nil)

(defun iiimcf-dispatch-emacs-event
  (events &optional buf save-excursion-p)
  (setq unread-command-events
	(nconc unread-command-events events))
  (if buf
      (if save-excursion-p
	  (save-excursion
	    (set-buffer buf)
	    (iiimcf-dispatch-emacs-event-internal))
	(let ((oldbuf (current-buffer)))
	  (set-buffer buf)
	  (iiimcf-dispatch-emacs-event-internal)
	  (set-buffer oldbuf)))
    (iiimcf-dispatch-emacs-event-internal)))

;;;
;;; LWE
;;;

;; Action alist for LWE facilities.
(defconst iiimcf-lwe-action-alist
  '((mode-switch . "goto")
    (convert . "convert")
    (backspace . "backspace")
    (convert-s . "convert-s")
    (unconvert . "unconvert")
    (next . "next")
    (next-s . "next-s")
    (previous . "previous")
    (previous-s . "previous-s")
    (forward . "forward")
    (backword . "backword")
    (move-top . "move-top")
    (move-bottom . "move-bottom")
    (clear . "clear")
    (expand . "expand")
    (expand-s . "expand-s")
    (shrink . "shrink")
    (shrink-s . "shrink-s")
    (expand-noconv . "expand-noconv")
    (expand-noconv-s . "expand-noconv-s")
    (shrink-noconv . "shrink-noconv")
    (shrink-noconv-s . "shrink-noconv-s")
    (fix . "fix")
    ;; Maybe added in the future?
    ))

;;;
;;; Client descriptor.
;;;

(defun iiimcf-make-client-descriptor (appname)
  (let ((arch "*")
	(vendor "*")
	(osname "*"))
    (if (string-match "\\([^\\-]+\\)-\\([^\\-]+\\)-\\(.+\\)"
		      system-configuration)
	(setq arch (match-string 1 system-configuration)
	      vendor (match-string 2 system-configuration)
	      osname (match-string 3 system-configuration)))
    (mapcar
     (lambda (x)
       (if (or (string= (symbol-value x) "*")
	       (string= (symbol-value x) ""))
	   (set x "Unknown")))
     '(arch vendor osname))

  (list
   'client-descriptor
   (list
    (format "%s IIIMECF/%s Emacs/%s"
	    appname iiimcf-version emacs-version)
    osname
    arch
    "Unknown"))))

(defun iiimcf-send-client-descriptor (com-id im-id appname)
  (iiimp-send-message
   'iiimp-im-setimvalues
   com-id im-id
   (list
    (iiimcf-make-client-descriptor appname)))
  (iiimcf-message-manager
   com-id
   (list
    (list 'iiimp-im-setimvalues-reply
	  com-id im-id))))

;;;
;;; get IM attribute.
;;;

(defun iiimcf-get-im-attributes (com-id im-id attrs)
  (iiimp-send-message
   'iiimp-im-getimvalues
   com-id im-id attrs)
  (iiimcf-message-manager
   com-id
   (list (list 'iiimp-im-getimvalues-reply com-id im-id))))

;;;
;;; handle database
;;;

;; ((<COM-ID> . A-LIST) ...)
(defvar iiimcf-com-alist nil)
;; ((<IM-ID> . A-LIST) ...)
(defvar iiimcf-im-alist nil)
;; ((<IC-ID> . A-LIST) ...)
(defvar iiimcf-ic-alist nil)

;;
;; register
;;

(defun iiimcf-register-com-id (com-id)
  (if (null (assq com-id iiimcf-com-alist))
      (setq iiimcf-com-alist
	    (cons
	     (list com-id)
	     iiimcf-com-alist))))

(defun iiimcf-register-im-id (im-id)
  (if (null (assoc im-id iiimcf-im-alist))
      (setq iiimcf-im-alist
	    (cons
	     (list im-id)
	     iiimcf-im-alist))))

(defun iiimcf-register-ic-id (ic-id)
  (if (null (assoc ic-id iiimcf-ic-alist))
      (setq iiimcf-ic-alist
	    (cons
	     (list ic-id)
	     iiimcf-ic-alist))))

;;
;; unregister
;;

(defun iiimcf-unregister-com-id (com-id)
  (let ((slot (assq com-id iiimcf-com-alist)))
    (if slot (setq iiimcf-com-alist (delq slot iiimcf-com-alist)))))

(defun iiimcf-unregister-im-id (im-id)
  (let ((slot (assoc im-id iiimcf-im-alist)))
    (if slot (setq iiimcf-im-alist (delq slot iiimcf-im-alist)))))

(defun iiimcf-unregister-ic-id (ic-id)
  (let ((slot (assoc ic-id iiimcf-ic-alist)))
    (if slot (setq iiimcf-ic-alist (delq slot iiimcf-ic-alist)))))

;;
;; get value
;;

(defsubst iiimcf-com-id-get (com-id key)
  (cdr (assq key (cdr (assq com-id iiimcf-com-alist)))))

(defsubst iiimcf-im-id-get (im-id key)
  (cdr (assq key (cdr (assoc im-id iiimcf-im-alist)))))

(defsubst iiimcf-ic-id-get (ic-id key)
  (cdr (assq key (cdr (assoc ic-id iiimcf-ic-alist)))))

;;
;; put value
;;

(defun iiimcf-com-id-put (com-id key val)
  (let ((slot (assq com-id iiimcf-com-alist))
	sslot)
    (if slot
	(if (setq sslot (assq key (cdr slot)))
	    (setcdr sslot val)
	  (setcdr slot (cons (cons key val) (cdr slot))))
      (error "COM-ID:%S is not registerd." com-id))))

(defun iiimcf-im-id-put (im-id key val)
  (let ((slot (assoc im-id iiimcf-im-alist))
	sslot)
    (if slot
	(if (setq sslot (assq key (cdr slot)))
	    (setcdr sslot val)
	  (setcdr slot (cons (cons key val) (cdr slot))))
      (error "IM-ID:%S is not registerd." im-id))))

(defun iiimcf-ic-id-put (ic-id key val)
  (let ((slot (assoc ic-id iiimcf-ic-alist))
	sslot)
    (if slot
	(if (setq sslot (assq key (cdr slot)))
	    (setcdr sslot val)
	  (setcdr slot (cons (cons key val) (cdr slot))))
      (error "IC-ID:%S is not registerd." ic-id))))

;;
;; map value
;;

(defun iiimcf-map-all-coms (func)
  (let ((ll iiimcf-com-alist))
    (while ll
      (funcall func (car (car ll)))
      (setq ll (cdr ll)))))

(defun iiimcf-map-all-ims (func com-id)
  (let ((ll iiimcf-im-alist)
	im-id)
    (while (setq im-id (car (car ll)))
      (if (eq (iiimp-im-id-to-com-id im-id) com-id)
	  (funcall func im-id))
      (setq ll (cdr ll)))))

(defun iiimcf-map-all-ics (func im-id)
  (let ((ll iiimcf-ic-alist)
	ic-id)
    (while (setq ic-id (car (car ll)))
      (if (equal (iiimp-ic-id-to-im-id ic-id) im-id)
	  (funcall func ic-id))
      (setq ll (cdr ll)))))

;;;
;;; Event manager.
;;;
;;   IIIMCF for Emacs provides a simple message dispatcher
;; like generic function, but it is a very fragile version.
;; MESSAGE format is a list like the following.
;;     (<MESSAGE-TYPE> <MESSAGE-DEPENDENT-VALUES>...)
;;

(defvar iiimcf-event-before-handler-alist
  nil)

(defvar iiimcf-event-normal-handler-alist
  nil)

(defvar iiimcf-event-after-handler-alist
  nil)

(defvar iiimcf-event-handler-alist nil)

(defvar iiimcf-pumped-message-list nil)

(defun iiimcf-register-handler
  (function specializer &optional qualifier)
  (let* ((hl (cond ((eq qualifier :before)
		    'iiimcf-event-before-handler-alist)
		   ((eq qualifier :after)
		    'iiimcf-event-after-handler-alist)
		   (t
		    'iiimcf-event-normal-handler-alist)))
	 (slot (cons specializer function))
	 (prob (member slot iiimcf-event-handler-alist)))
    (if (null prob)
	(set hl (cons slot (symbol-value hl))))
    (setq iiimcf-event-handler-alist
	  (append
	   iiimcf-event-before-handler-alist
	   iiimcf-event-normal-handler-alist
	   iiimcf-event-after-handler-alist))
    nil))

(defun iiimcf-remove-handler
  (function specializer &optional qualifier)
  "Remove the specified handler from IIIMCF event manager.
If you specify nil to FUNCTION or SPECIALIZER, remove
all handlers that match the rest."
  (let ((sym (cond ((eq qualifier :before)
		    'iiimcf-event-before-handler-alist)
		   ((eq qualifier :after)
		    'iiimcf-event-after-handler-alist)
		   (t
		    'iiimcf-event-normal-handler-alist)))
	slot ll)
    (cond ((and function specializer)
	   (setq slot (cons specializer function))
	   (set sym (delete slot (symbol-value sym))))
	  (function
	   (setq ll (symbol-value sym))
	   (while
	       (prog1
		   (setq slot (rassq function ll))
		 (setq ll (delq slot ll))))
	   (set sym ll))
	  (specializer
	   (setq ll (symbol-value sym))
	   (while
	       (prog1
		   (setq slot (assoc specializer ll))
		 (setq ll (delq slot ll))))
	   (set sym ll))
	  (t
	   (set sym nil)))
    (setq iiimcf-event-handler-alist
	  (append
	   iiimcf-event-before-handler-alist
	   iiimcf-event-normal-handler-alist
	   iiimcf-event-after-handler-alist))
    nil))

;; Should we cache the result for efficiency?
(defsubst iiimcf-message-match-specializer-p
  (obj specializer)
  (while
      (and
       (or (equal (car obj) (car specializer))
	   (eq (car specializer) 'any))
       (setq obj (cdr obj)
	     specializer (cdr specializer))))
  (null specializer))

(defun iiimcf-message-manager (com-id w-specs)
  (let ((pumped-message-list
	 nil) ; iiimcf-pumped-message-list)
	(exit (null w-specs))
	ws mes ret hl hle func applied newcell)
    (while (setq mes (iiimp-wait-message
		      com-id (not exit)))
      (setq newcell (cons mes nil))
      (if pumped-message-list
	  (setcdr pumped-message-list
		  newcell))
      (setq pumped-message-list newcell)
      (iiimp-add-debug-log (format "<- %S\n" mes))
      (setq hl iiimcf-event-handler-alist
	    applied nil)
      (dolist (hle hl)
	(if (iiimcf-message-match-specializer-p mes (car hle))
	    (progn
	      (setq func (cdr hle))
	      (if (not (memq func applied))
		  (progn
		    (funcall func mes)
		    (setq applied (cons func applied)))))))
      (while (and (not exit)
		  (progn
		    (setq ws w-specs
			  mes (car pumped-message-list))
		    (while
			(and ws
			     (if (iiimcf-message-match-specializer-p
				  mes (car ws))
				 (progn
				   (setq exit t
					 ret mes)
				   nil)
			       (setq ws (cdr ws))
			       t)))
		    (and (cdr pumped-message-list)
			 (setq pumped-message-list
			       (cdr pumped-message-list)))))))
    ret))

(defun iiimcf-im-trigger-notify-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-trigger-notify-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-trigger-notify-handler-default)
 '(iiimp-im-trigger-notify)
 :after)

(defun iiimcf-im-setimvalues-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-setimvalues-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-setimvalues-handler-default)
 '(iiimp-im-setimvalues)
 :after)

(defun iiimcf-im-forward-event-handler-default (mes)
  (iiimp-add-debug-log "REPLY:\n")
  (iiimp-send-message
   'iiimp-im-forward-event-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-forward-event-handler-default)
 '(iiimp-im-forward-event)
 :after)

;; OPERATIONS that have been processed are removed from
;; the corresponding MES slot, which is (nth 5 mes).
(defun iiimcf-im-forward-event-with-operations-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-forward-event-with-operations-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)
   (iiimp-message-forward-event-operations mes)))
(iiimcf-register-handler
 (function iiimcf-im-forward-event-handler-default)
 '(iiimp-im-forward-event-with-operations)
 :after)

(defun iiimcf-im-preedit-start-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-preedit-start-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-preedit-start-handler-default)
 '(iiimp-im-preedit-start)
 :after)

(defun iiimcf-im-preedit-draw-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-preedit-draw-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-preedit-draw-handler-default)
 '(iiimp-im-preedit-draw)
 :after)

(defun iiimcf-im-preedit-done-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-preedit-done-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-preedit-done-handler-default)
 '(iiimp-im-preedit-done)
 :after)

(defun iiimcf-im-status-start-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-status-start-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-status-start-handler-default)
 '(iiimp-im-status-start)
 :after)

(defun iiimcf-im-status-draw-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-status-draw-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-status-draw-handler-default)
 '(iiimp-im-status-draw)
 :after)

(defun iiimcf-im-status-done-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-status-done-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-status-done-handler-default)
 '(iiimp-im-status-done)
 :after)

;;; LOOKUP default handlers

(defun iiimcf-im-lookup-choice-start-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-lookup-choice-start-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-lookup-choice-start-handler-default)
 '(iiimp-im-lookup-choice-start)
 :after)

(defun iiimcf-im-lookup-choice-draw-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-lookup-choice-draw-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-lookup-choice-draw-handler-default)
 '(iiimp-im-lookup-choice-draw)
 :after)

(defun iiimcf-im-lookup-choice-process-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-lookup-choice-process-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-lookup-choice-process-handler-default)
 '(iiimp-im-lookup-choice-process)
 :after)

(defun iiimcf-im-lookup-choice-done-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-lookup-choice-done-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-lookup-choice-done-handler-default)
 '(iiimp-im-lookup-choice-done)
 :after)


;;; AUX default handlers

(defun iiimcf-im-aux-start-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-aux-start-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)
   (iiimp-message-aux-window-id mes)
   (iiimp-message-aux-im-name mes)))
(iiimcf-register-handler
 (function iiimcf-im-aux-start-handler-default)
 '(iiimp-im-aux-start)
 :after)

(defun iiimcf-im-aux-draw-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-aux-draw-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)
   (iiimp-message-aux-window-id mes)
   (iiimp-message-aux-im-name mes)))
(iiimcf-register-handler
 (function iiimcf-im-aux-draw-handler-default)
 '(iiimp-im-aux-draw)
 :after)

(defun iiimcf-im-aux-done-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-aux-done-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)
   (iiimp-message-aux-window-id mes)
   (iiimp-message-aux-im-name mes)))
(iiimcf-register-handler
 (function iiimcf-im-aux-done-handler-default)
 '(iiimp-im-aux-done)
 :after)

;;; HOTKEY default handler

(defun iiimcf-im-hotkey-notify-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-hotkey-notify-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-hotkey-notify-handler-default)
 '(iiimp-im-hotkey-notify)
 :after)

(defun iiimcf-im-hotkey-state-notify-handler-default (mes)
  (iiimp-send-message
   'iiimp-im-hotkey-state-notify-reply
   (iiimp-message-com-id mes)
   (iiimp-message-im-id mes)
   (iiimp-message-ic-id mes)))
(iiimcf-register-handler
 (function iiimcf-im-hotkey-state-notify-handler-default)
 '(iiimp-im-hotkey-state-notify)
 :after)


;;;
;;; create/delete handles
;;;

(defun iiimcf-connect-com (proto host port)
  (let ((com-id
	 (iiimp-create-network-channel proto host port)))
    (iiimcf-register-com-id com-id)
    com-id))

(defun iiimcf-destroy-com (com-id)
  (iiimcf-map-all-ims
   (function iiimcf-disconnect-im)
   com-id)
  (iiimcf-unregister-com-id com-id)
  (iiimp-destroy-network-channel com-id)
  nil)

(defun iiimcf-connect-im (com-id username &optional client-descriptor)
  (let (mes im-id)
    (iiimp-send-message
     'iiimp-im-connect com-id username)
    (setq mes (iiimcf-message-manager
	       com-id
	       (list
		(list 'iiimp-im-connect-reply com-id))))
    (setq im-id (iiimp-message-im-id mes))
    (iiimcf-register-im-id im-id)
;    (if client-descriptor
;	(iiimcf-send-client-descriptor
;	 com-id im-id client-descriptor))
    im-id))

(defun iiimcf-disconnect-im (im-id)
  (iiimcf-map-all-ics
   (function iiimcf-destroy-ic)
   im-id)
  (if (iiimp-check-channel-connection
       (iiimp-im-id-to-com-id im-id))
      (progn
	(iiimp-send-message
	 'iiimp-im-disconnect
	 (iiimp-im-id-to-com-id im-id)
	 im-id)
	(iiimcf-message-manager
	 (iiimp-im-id-to-com-id im-id)
	 (list (list 'iiimp-im-disconnect-reply
		     (iiimp-im-id-to-com-id im-id) im-id)))))
  (iiimcf-unregister-im-id im-id)
  nil)

(defun iiimcf-create-ic (im-id attrs)
  (let ((com-id (iiimp-im-id-to-com-id im-id))
	mes ic-id)
    (iiimp-send-message
     'iiimp-im-createic
     com-id im-id attrs)
    (setq mes
	  (iiimcf-message-manager
	   com-id
	   (list
	    (list 'iiimp-im-createic-reply
		  com-id im-id)))
	  ic-id (iiimp-message-ic-id mes))
    (iiimcf-register-ic-id ic-id)
    ic-id))

(defun iiimcf-destroy-ic (ic-id)
  (if (iiimp-check-channel-connection
       (iiimp-ic-id-to-com-id ic-id))
      (progn
	(iiimp-send-message
	 'iiimp-im-destroyic
	 (iiimp-ic-id-to-com-id ic-id)
	 (iiimp-ic-id-to-im-id ic-id)
	 ic-id)
	(iiimcf-message-manager
	 (iiimp-ic-id-to-com-id ic-id)
	 (list
	  (list 'iiimp-im-destroyic-reply
		(iiimp-ic-id-to-com-id ic-id)
		(iiimp-ic-id-to-im-id ic-id)
		ic-id)))))
  (iiimcf-unregister-ic-id ic-id)
  nil)

;;;
;;; ic focus
;;;

(defun iiimcf-set-icfocus (ic-id)
  (progn
    (iiimp-send-message
     'iiimp-im-seticfocus
     (iiimp-ic-id-to-com-id ic-id)
     (iiimp-ic-id-to-im-id ic-id)
     ic-id)
    (iiimcf-message-manager
     (iiimp-ic-id-to-com-id ic-id)
     (list
      (list 'iiimp-im-seticfocus-reply
	    (iiimp-ic-id-to-com-id ic-id)
	    (iiimp-ic-id-to-im-id ic-id)
	    ic-id)))))

(defun iiimcf-unset-icfocus (ic-id)
  (progn
    (iiimp-send-message
     'iiimp-im-unseticfocus
     (iiimp-ic-id-to-com-id ic-id)
     (iiimp-ic-id-to-im-id ic-id)
     ic-id)
    (iiimcf-message-manager
     (iiimp-ic-id-to-com-id ic-id)
     (list
      (list 'iiimp-im-unseticfocus-reply
	    (iiimp-ic-id-to-com-id ic-id)
	    (iiimp-ic-id-to-im-id ic-id)
	    ic-id)))))

;;;
;;; forward event
;;;

(defun iiimcf-forward-event (ic-id ev)
  (let ((iiimev (if (stringp ev) ev
		  (iiimcf-translate-emacs-event ev))))
    (if iiimev
	(progn
	  (iiimp-send-message
	   'iiimp-im-forward-event
	   (iiimp-ic-id-to-com-id ic-id)
	   (iiimp-ic-id-to-im-id ic-id)
	   ic-id
	   (if (stringp iiimev)
	       (list 'string iiimev)
	     (list 'keyevent (list iiimev))))
	   ;; I have no idea why Solaris 8 IIIM server
	   ;; does not respond to IM_FORWARD_EVENT sometimes.
	  (iiimcf-message-manager
	   (iiimp-ic-id-to-com-id ic-id)
	   (list
	    (list 'iiimp-im-forward-event-reply
		  (iiimp-ic-id-to-com-id ic-id)
		  (iiimp-ic-id-to-im-id ic-id)
		  ic-id)
	    (list 'iiimp-im-forward-event
		  (iiimp-ic-id-to-com-id ic-id)
		  (iiimp-ic-id-to-im-id ic-id)
		  ic-id)))
	  nil)
      ev)))

;;;
;;; toggle IM(notify trigger)
;;;

(defsubst iiimcf-trigger-state (ic-id)
  (iiimcf-ic-id-get ic-id 'trigger-state))

(defun iiimcf-notify-trigger (ic-id flag)
  (if flag (setq flag t))
  (if (eq (iiimcf-trigger-state ic-id) flag)
      nil
    (iiimp-send-message
     'iiimp-im-trigger-notify
     (iiimp-ic-id-to-com-id ic-id)
     (iiimp-ic-id-to-im-id ic-id)
     ic-id flag)
    (iiimcf-message-manager
     (iiimp-ic-id-to-com-id ic-id)
     (list
      (list 'iiimp-im-trigger-notify-reply
	    (iiimp-ic-id-to-com-id ic-id)
	    (iiimp-ic-id-to-im-id ic-id)
	    ic-id)))
    (iiimcf-ic-id-put ic-id 'trigger-state flag)))

;;;
;;; toggle LE (hotkey)
;;;

;; Profile     Hotkey-ID
;; 0           0 (Unit Hotkey Test) 3 (LE switch) 4 (trigger keys)
;; 1           1 (compose_key) 2 (dead key) 3 (LE switch) 4 (trigger keys)
;; 2 (default) 3 (LE switch) 4 (trigger keys)

(defun iiimcf-notify-le-switch (ic-id)
  (let ((hotkey-id
	 (nth 0 (iiimcf-server-control-get-hotkeys-by-label
		 (iiimp-ic-id-to-im-id ic-id)
		 "LE SWITCH"))))
  (iiimcf-notify-hotkey ic-id hotkey-id 1)))

(defun iiimcf-notify-hotkey (ic-id hotkey-id hotkey-idx)
  (iiimp-send-message
   'iiimp-im-hotkey-notify
   (iiimp-ic-id-to-com-id ic-id)
   (iiimp-ic-id-to-im-id ic-id)
   ic-id hotkey-id hotkey-idx)
  (iiimcf-message-manager
   (iiimp-ic-id-to-com-id ic-id)
   (list
    (list 'iiimp-im-hotkey-notify-reply
	  (iiimp-ic-id-to-com-id ic-id)
	  (iiimp-ic-id-to-im-id ic-id)
	  ic-id))))

;;;
;;; maintain preedit
;;;

(defsubst iiimcf-get-current-preedit-text (ic-id)
  (cdr (iiimcf-ic-id-get ic-id 'preedit)))

(defsubst iiimcf-get-current-preedit-caret (ic-id)
  (car (iiimcf-ic-id-get ic-id 'preedit)))

(defsubst iiimcf-get-current-preedit (ic-id)
  (iiimcf-ic-id-get ic-id 'preedit))

(defun iiimcf-surpos-to-charpos (str pos)
  "Convert from surrogate position to char position."
  (let ((i 0))
    (while (and (< i (length str))
		(< 0 pos))
      (if (< ?\xffff (char-to-ucs (aref str i)))
	  (setq pos (- pos 2))
	(setq pos (- pos 1)))
      (setq i (1+ i)))
    (if (/= pos 0) (error "invalid pos (larger than string length or at the surrogate border)."))
    i))

(defun iiimcf-update-preedit (mes)
  (let* ((ic-id (iiimp-message-ic-id mes))
	 (preedit-text (iiimcf-get-current-preedit-text
			ic-id))
	 (data (iiimp-message-preedit-draw-data mes))
	 (caret (aref data 0))
	 (ch-first (iiimcf-surpos-to-charpos
		    preedit-text (aref data 1)))
	 (ch-len (aref data 2))
	 (contents (aref data 3))
	 (str (nth 1 contents))
	 (ch-second (iiimcf-surpos-to-charpos
		     preedit-text
		     (+ (aref data 1) ch-len)))
	 (len (length preedit-text))
	 head tail)
    (if (and (>= len ch-first)
	     (> ch-first 0))
	(setq head (substring
		    preedit-text
		    0 ch-first)))
    (if (> len ch-second)
	(setq tail (substring
		    preedit-text
		    ch-second)))
    (setq str (concat head str tail))
    (iiimcf-ic-id-put
     ic-id
     'preedit
     (cons (iiimcf-surpos-to-charpos str caret)
	   (if (> (length str) 0) str nil)))))
(iiimcf-register-handler
 (function iiimcf-update-preedit)
 '(iiimp-im-preedit-draw)
 :before)

;;;
;;; maintain lookup choice.
;;;
;; 'lookup-choice-config
;; 'lookup-choice
;; 'candidates

(defun iiimcf-set-lookup-choice-config (mes)
  (let ((ic-id (iiimp-message-ic-id mes)))
    (iiimcf-ic-id-put
     ic-id 'lookup-choice-config
     (iiimp-message-lookup-choice-start-data mes))))
(iiimcf-register-handler
 (function iiimcf-set-lookup-choice-config)
 '(iiimp-im-lookup-choice-start)
 :before)
(defun iiimcf-update-lookup-choice (mes)
  (let* ((ic-id (iiimp-message-ic-id mes))
	 (lcdd (iiimp-message-lookup-choice-draw-data mes)))
    (iiimcf-ic-id-put
     ic-id 'lookup-choice lcdd)
    (iiimcf-ic-id-put
     ic-id 'candidates (aref lcdd 3))))
(iiimcf-register-handler
 (function iiimcf-update-lookup-choice)
 '(iiimp-im-lookup-choice-draw)
 :before)
(defun iiimcf-update-lookup-choice-process (mes)
  (let* ((ic-id (iiimp-message-ic-id mes))
	 (type (iiimp-message-lookup-choice-process-type mes))
	 (val (iiimp-message-lookup-choice-process-value mes))
	 (lcdd (iiimcf-ic-id-get ic-id 'lookup-choice))
	 (lccfg (iiimcf-ic-id-get ic-id 'lookup-choice-config))
	 (num (aref lccfg 1))
	 (idx (aref lcdd 2)))
    (cond ((eq type 'index)
	   (setq idx (iiimp-message-lookup-choice-process-value mes)))
	  ((eq val 'next)
	   (setq idx (+ idx num)))
	  ((eq val 'prev)
	   (setq idx (- idx num)))
	  ((eq val 'first)
	   (setq idx (aref lcdd 0)))
	  ((eq val 'last)
	   (setq idx (- (aref lcdd 1) num -1))))
    (aset lcdd 2 idx)))
(iiimcf-register-handler
 (function iiimcf-update-lookup-choice-process)
 '(iiimp-im-lookup-choice-process)
 :before)
(defun iiimcf-clear-lookup-choice (mes)
  (let ((ic-id (iiimp-message-ic-id mes)))
    (iiimcf-ic-id-put ic-id 'lookup-choice nil)
    (iiimcf-ic-id-put ic-id 'lookup-choice-config nil)))
(iiimcf-register-handler
 (function iiimcf-clear-lookup-choice)
 '(iiimp-im-lookup-choice-start)
 :before)

;;;

(provide 'iiimcf)

;; iiimcf.el ends here.

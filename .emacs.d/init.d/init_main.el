;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定など
(require 'init_setting)
;; install-elisp & auto-install
(require 'init_auto-install)

;; 旧設定
(load "~/_emacs")

;; ruby-mode
(require 'init_ruby)
;; rails-mode
(require 'init_rails)
;; haml-mode/sass-mode
(require 'init_haml)
;; yaml-mode
(require 'init_yaml)

;; howm
(require 'init_howm)
;; session.el
(require 'init_session)
;;;; outputz.el
;;(require 'init_outputz)

;; gauche.el
(require 'init_gauche)

;; anything.el
(require 'init_anything)
(require 'init_popwin)

;; python-mode
(require 'init_python)
;; elscreen
(require 'init_elscreen)
;; howm
(require 'init_howm)

;; ReVIEW
(require 'init_review)

;; mozc
(when x-window-p
  (require 'init_mozc))

;; evernote
(require 'init_evernote)

;; multi-term
(require 'init_multi-term)

;; markdown
(require 'init_markdown)

(provide 'init_main)
;; init_mail.el ends here

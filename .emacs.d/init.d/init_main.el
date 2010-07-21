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

;; howm
(require 'init_howm)
;; session.el
(require 'init_session)
;; outputz.el
(require 'init_outputz)

;; gauche.el
(require 'init_gauche)

;; anything.el
(require 'init_anything)

;; python-mode
(require 'init_python)
;; elscreen
(require 'init_elscreen)
;; howm
(require 'init_howm)

(provide 'init_main)
;; init_mail.el ends here

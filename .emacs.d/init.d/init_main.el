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
;; ruby-mode
(require 'init_rails)
;; howm
(require 'init_howm)
;; session.el
(require 'init_session)
;; outputz.el
(require 'init_outputz)

;; anything.el
(require 'init_anything)

;; python-mode
(require 'init_python)

(provide 'init_main)
;; init_mail.el ends here

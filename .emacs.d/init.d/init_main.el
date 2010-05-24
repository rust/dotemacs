;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定など
(require 'init_setting)
;; install-elisp & auto-install
(require 'init_auto-install)
;; ruby-mode
(require 'init_ruby)
;; anything.el
(require 'init_anything)



;; 旧設定
(load "~/_emacs")

(provide 'init_main)
;; init_mail.el ends here
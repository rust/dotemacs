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
;; coffee-mode
(require 'init_coffee_script)
;; yaml-mode
(require 'init_yaml)
;; web-mode
(require 'init_web-mode)

;;;; howm
(require 'init_howm)
;; session.el
(require 'init_session)

;; gauche.el
(require 'init_gauche)

;; helm
;;(require 'init_helm)

;; python-mode
(require 'init_python)
;; elscreen
(require 'init_elscreen)

;; ReVIEW
(require 'init_review)

;; multi-term
(require 'init_multi-term)

;; markdown
(require 'init_markdown)

;; diff-mode
(require 'init_diff)

;; yasnippet
(require 'init_yasnippet)

;; git
(require 'init_git)

;; anzu
(require 'init_anzu)

(provide 'init_main)
;; init_mail.el ends here
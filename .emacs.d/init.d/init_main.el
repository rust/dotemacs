;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定など
(require 'init_setting)

;; misc
(require 'init_misc)

;; auto-complete
(require 'init_auto-complete)

;; ruby-mode
(require 'init_ruby)
;; rails-mode
(require 'init_rails)
;; for templates
(require 'init_templates)
;; coffee-mode
(require 'init_coffee_script)
;; javascript mode
(require 'init_javascript)
;; yaml-mode
(require 'init_yaml)
;; web-mode
(require 'init_web-mode)
;; Programing Languages
(require 'init_progs)

;; howm
(require 'init_howm)
;; session.el
(require 'init_session)

;; gauche.el
(require 'init_gauche)

;; helm
(require 'init_helm)

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

;; migemo
(require 'init_migemo)

;; elixir
(require 'init_elixir)

;; For infra
(require 'init_infra)

(provide 'init_main)
;; init_mail.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定など
(require 'init_setting)

;; misc
(require 'init_misc)

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
;; Programing Languages
(require 'init_progs)

;; helm
(require 'init_helm)

;; python-mode
(require 'init_python)

;; ReVIEW
(require 'init_review)

;; markdown
(require 'init_markdown)

;; git
(require 'init_git)

;; For infra
(require 'init_infra)

(provide 'init_main)
;; init_mail.el ends here

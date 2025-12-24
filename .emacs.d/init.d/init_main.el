;;; init_main.el --- メイン設定ファイルの読み込み  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shin-ichiro OGAWA <rust@stnard.jp>

;;; Commentary:

;; 各種設定ファイルを読み込むメインモジュール。
;; プログラミング言語の設定、Git、LSP、AIなどの設定を含む。

;;; Code:

;; 初期設定など
(require 'init_setting)

;; finder
(require 'init_finder)

;; misc
(require 'init_misc)

;; treesit
(require 'init_treesit)

;; ruby-mode
(require 'init_ruby)
;; rails-mode
(require 'init_rails)
;; for templates
(require 'init_templates)
;; javascript mode
(require 'init_javascript)
;; yaml-mode
(require 'init_yaml)
;; Programing Languages
(require 'init_progs)
;; LSP
(require 'init_lsp)

;; python-mode
(require 'init_python)

;; markdown
(require 'init_markdown)

;; git
(require 'init_git)

;; For infra
(require 'init_infra)

;; AI
(require 'init_ai)

(provide 'init_main)
;;; init_main.el ends here

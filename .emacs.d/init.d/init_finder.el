;;; init_finder.el --- ファイル検索・選択の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Verticoなどのファイル検索・選択ツールの設定。

;;; Code:

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode))

(provide 'init_finder)
;; init_finder.el ends here

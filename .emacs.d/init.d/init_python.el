;;; init_python.el --- Python設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Python開発環境の設定。
;; Emacs 29+ 組み込みの python-ts-mode を使用。

;;; Code:

;; python-ts-mode（Emacs 29+ 組み込み、python-mode から移行）
(use-package python
  :ensure nil
  :defer t
  :mode (("\\.py\\'" . python-ts-mode)))

(provide 'init_python)
;; init_python.el ends here

;;; init_yaml.el --- YAML設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; YAMLファイル編集のための設定。
;; Emacs 29+ 組み込みの yaml-ts-mode を使用。

;;; Code:

;; yaml-ts-mode（Emacs 29+ 組み込み、yaml-mode から移行）
(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :mode (("\\.yaml$" . yaml-ts-mode)
         ("\\.yml$"  . yaml-ts-mode)))

(provide 'init_yaml)
;; init_yaml.el ends here

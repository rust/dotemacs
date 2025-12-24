;;; init_terminal-mode.el --- ターミナルモード設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shinichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; ターミナル環境での設定。
;; ツールバーやメニューバーの非表示設定を含む。

;;; Code:

;; initialization in Terminal

(tool-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "C-m") 'newline-and-indent)

(provide 'init_terminal-mode)
;; init_terminal-mode.el ends here

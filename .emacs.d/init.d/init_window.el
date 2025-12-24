;;; init_window.el --- ウィンドウシステム設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shinichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; ウィンドウシステム環境の設定。
;; フレーム設定、ツールバー、フォントなどの設定を含む。

;;; Code:

;; initialization for Window System

;; hide menu
(tool-bar-mode -1)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; Font: Moralerspace
(add-to-list 'default-frame-alist '(font . "Moralerspace Neon HWJPDOC-10"))

;; transparent
(add-to-list 'default-frame-alist '(alpha . 100))
(set-frame-parameter nil 'alpha 100)
(setq frame-alpha-lower-limit 95)

(setq hl-line-face 'underline)
(global-hl-line-mode)

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(cond
 (mac-p (require 'init_mac)))

(load-theme 'solarized-light t)

(provide 'init_window)
;; init_window.el ends here

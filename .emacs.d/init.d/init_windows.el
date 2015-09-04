;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_windows.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hide menu
(display-time-mode t)
(tool-bar-mode -1)
(transient-mark-mode t)
(menu-bar-mode -1)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; Font: CodeM
(add-to-list 'default-frame-alist '(font . "CodeM-09"))

(require 'solarized-theme)
(setq solarized-termcolors 256)
(setq solarized-degrade t)
;; (setq solarized-contrast 'high)
(setq solarized-visibility 'high)
(setq solarized-broken-srgb t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode -1)
 '(show-paren-mode t))

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; disable scroll-bar
(set-scroll-bar-mode nil)

(setq magit-git-executable "C:/Program Files (x86)/Git/cmd/git.exe")

(provide 'init_windows)
;; init_windows.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_window.el

;; Copyright (C) 2025 Shinichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization for Window System

;; hide menu
(tool-bar-mode -1)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; Font: Moralerspace
(add-to-list 'default-frame-alist '(font . "Moralerspace Neon HWJPDOC-10"))

(load-theme 'wombat t)

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

(provide 'init_window)
;; init_window.el ends here

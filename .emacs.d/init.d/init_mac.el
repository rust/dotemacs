;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Color
(set-frame-parameter (selected-frame) 'alpha '(97 75))
;; metakey
;; (setq ns-command-modifier (quote meta))
;; (setq ns-alternate-modifier (quote super))

;; hide menu
(display-time-mode t)
(tool-bar-mode -1)
(transient-mark-mode t)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; フォント
(create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
(set-fontset-font "fontset-ricty"
                  'unicode
                  (font-spec :family "Ricty" :size 14)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-ricty"))

(require 'color-theme)
(color-theme-initialize)
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/emacs-color-theme-solarized")
(setq solarized-termcolors 256)
(setq solarized-degrade t)
(setq solarized-contrast 'high)
(setq solarized-visibility 'high)
(setq solarized-broken-srgb t)
(if macbook-air-p (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t))

;; high-light current line
(defface hlline-face
  '((((class color)
      (background dark))
     ;;(:background "dark state gray"))
     (:background "gray10"
                  :underline nil))
    (((class color)
      (background light))
     (:background "ForestGreen"
                  :underline nil))
    (t ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;;(setq hl-line-face 'underline)
;; (global-hl-line-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t))

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(when (>= emacs-major-version 23)
  (define-key global-map [165] nil)
  (define-key global-map [67109029] nil)
  (define-key global-map [134217893] nil)
  (define-key global-map [201326757] nil)
  (define-key function-key-map [165] [?\\])
  (define-key function-key-map [67109029] [?\C-\\])
  (define-key function-key-map [134217893] [?\M-\\])
  (define-key function-key-map [201326757] [?\C-\M-\\]))

;; disable scroll-bar
(set-scroll-bar-mode nil)

(provide 'init_mac)
;; init_mac.el ends here

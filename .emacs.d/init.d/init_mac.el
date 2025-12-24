;;; init_mac.el --- macOS固有設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; macOS環境での固有設定。
;; フォント設定やmacOS固有のキーバインドを含む。

;;; Code:

;; Font: Moralerspace
(add-to-list 'default-frame-alist '(font . "Moralerspace Neon HWJPDOC-14"))

;; hide menu
(display-time-mode t)
(transient-mark-mode t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode -1)
 '(show-paren-mode t))

(define-key global-map [165] nil)
(define-key global-map [67109029] nil)
(define-key global-map [134217893] nil)
(define-key global-map [201326757] nil)
(define-key function-key-map [165] [?\\])
(define-key function-key-map [67109029] [?\C-\\])
(define-key function-key-map [134217893] [?\M-\\])
(define-key function-key-map [201326757] [?\C-\M-\\])

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-right-command-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; disable scroll-bar
(set-scroll-bar-mode nil)

(provide 'init_mac)
;; init_mac.el ends here

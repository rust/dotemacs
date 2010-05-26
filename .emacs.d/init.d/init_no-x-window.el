;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_no-x-window.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; メニューバーを消す
(menu-bar-mode nil)

;; 256 colors
(load "emacs21-256color-hack.el")
(require 'color-theme)
(load "my-color-theme")
(my-color-theme)

;; wb-line-number
(require 'wb-line-number)
(wb-line-number-toggle)
(set-scroll-bar-mode nil)
(setq wb-line-number-scroll-bar t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dired-header ((t (:foreground "aquamarine"))))
 '(font-lock-constant-face ((t (:foreground "purple"))))
 '(font-lock-function-name-face ((t (:foreground "#4186be" :weight extra-bold))))
 '(font-lock-keyword-face ((t (:foreground "#00ffff" :weight extra-bold))))
 '(font-lock-type-face ((t (:foreground "green" :weight extra-bold))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :weight bold))))
 '(erb-comment-delim-face ((t (:inherit erb-delim-face :foreground "red" :weight bold))))
 '(erb-comment-face ((t (:inherit erb-face :foreground "red" :weight bold))))
 '(erb-delim-face ((t (:background "#383838"))))
 '(erb-face ((t (:background "#383838"))))
 '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "pink4" :weight bold))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(howm-reminder-today-face ((t (:foreground "orange" :background "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black")))))

(provide 'init_no-x-window)
;; init_no-x-window.el ends here

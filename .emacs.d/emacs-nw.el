;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
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

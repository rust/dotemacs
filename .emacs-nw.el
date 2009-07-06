;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
;; メニューバーを消す
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(howm-list-normalizer (quote howm-view-sort-by-reverse-date) t)
 '(howm-list-prefer-word nil))

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

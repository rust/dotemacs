;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(howm-list-normalizer (quote howm-view-sort-by-reverse-date) t)
 '(howm-list-prefer-word nil)
 '(show-paren-mode t))

(tool-bar-mode nil)

(setq fontname (cond
                ((string-match "rust-worker" system-name)
                 "M+2VM+IPAG circle-8")
                ((string-match "precision" system-name)
                 "M+2VM+IPAG circle-9")))
(add-to-list 'default-frame-alist (cons 'font fontname))
(set-default-font fontname)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  (cons fontname "unicode-bmp"))

(require 'color-theme)
;; (load "my-color-theme-window")
;; (my-color-theme-window)
(color-theme-initialize)
(color-theme-hober)
;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_x-window.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-frame-alist
      (append (list '(foreground-color . "black"))))

(tool-bar-mode -1)

(when (>= emacs-major-version 23)
  (setq fontname (cond
                  ((string-match "rust-worker" system-name)
                   "M+2VM+IPAG circle-8")
                  ((string-match "netbook" system-name)
                   "M+2VM+IPAG circle-6")
                  ((string-match "precision" system-name)
                   "M+2VM+IPAG circle-9")
                  ((string-match "optiplex" system-name)
                   "M+2VM+IPAG circle-10")))
  (add-to-list 'default-frame-alist (cons 'font fontname))
  (set-default-font fontname)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    (cons fontname "unicode-bmp")))

(require 'color-theme)
;; (load "my-color-theme-window")
;; (my-color-theme-window)
(color-theme-initialize)
(color-theme-hober)

;; transparent
(add-to-list 'default-frame-alist '(alpha . 85))
(set-frame-parameter nil 'alpha 85)
(setq frame-alpha-lower-limit 85)

;; high-light current line
(defface hlline-face
  '((((class color)
      (background dark))
     ;;(:background "dark state gray"))
     (:background "gray10"
                  :underline "gray24"))
    (((class color)
      (background light))
     (:background "ForestGreen"
                  :underline nil))
    (t ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;;(setq hl-line-face 'underline)
(global-hl-line-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-header ((t (:foreground "aquamarine"))))
 '(erb-comment-delim-face ((t (:inherit erb-delim-face :foreground "red" :weight bold))))
 '(erb-comment-face ((t (:inherit erb-face :foreground "red" :weight bold))))
 '(erb-delim-face ((t (:background "#383838"))))
 '(erb-face ((t (:background "#383838"))))
 '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "pink4" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "purple"))))
 '(font-lock-function-name-face ((t (:foreground "#4186be" :weight extra-bold))))
 '(font-lock-keyword-face ((t (:foreground "#00ffff" :weight extra-bold))))
 '(font-lock-type-face ((t (:foreground "green" :weight extra-bold))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :weight bold))))
 '(howm-reminder-today-face ((t (:foreground "orange" :background "black"))))
 '(iiimcf-UI-preedit-reverse-face ((t (:foreground "DarkSlateGray1" :background "black"))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black"))))
 '(review-mode-bold-face ((t (:foreground "green1" :weight bold))))
 '(review-mode-bracket-face ((t (:foreground "purple1" :weight bold))))
 '(review-mode-underline-face ((t (:foreground "SkyBlue1" :underline t))))
 '(review-mode-underlinebold-face ((t (:foreground "DeepSkyBlue1" :underline t :weight bold)))))

;; Cookie
(setq w3m-use-cookies t)
;; favicon cache
(setq w3m-favicon-cache-expire-wait nil)

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; emoji
(add-to-list 'load-path "~/.emacs.d/emoji")
(require 'emoji)

;; skype
(add-to-list 'load-path "~/.emacs.d/skype")
(defun my-skype ()
  (interactive)
  (require 'skype)
  (setq skype--my-user-handle "stnard")
  (skype--init)
  (skype--open-all-users-buffer-command))

;;; git commit したときのバッファを utf-8 にする
(add-hook 'server-visit-hook
          (function (lambda ()
                      (if (string-match "COMMIT_EDITMSG" buffer-file-name)
                          (set-buffer-file-coding-system 'utf-8)))))
;;; fullscreen mode
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)

(provide 'init_x-window)
;; init_x-window.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_main.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Color
(set-frame-parameter nil 'alpha 85)
;; (if (= emacs-major-version 23)
;;   (setq initial-frame-alist '((width . 200)(height . 120)(top . 0)(left . (45)))))
(when (= emacs-major-version 22)
  (add-hook 'window-setup-hook
            (lambda ()
              (set-frame-parameter nil 'fullscreen 'fullboth)))
  (defun mac-toggle-max-window ()
    (interactive)
    (if (frame-parameter nil 'fullscreen)
        (set-frame-parameter nil 'fullscreen nil)
        (set-frame-parameter nil 'fullscreen 'fullboth)))
  (custom-set-variables
    '(display-time-mode t)
    '(tool-bar-mode nil)
    '(transitent-mark-mode t))
  (require 'linum)
  (global-linum-mode)
  (setq linum-format "%05d")
  (require 'carbon-font)
  (fixed-width-set-fontset "hiramaru" 12)
  (setq mac-command-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  (server-start)
  )

;; hide menu
(display-time-mode t)
(tool-bar-mode nil)
(transient-mark-mode t)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; フォントサイズ
(when (= emacs-major-version 22)
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing t)
  (require 'carbon-font))
(when (= emacs-major-version 23)
  ;; (create-fontset-from-ascii-font "M+2VM+IPAG circle-14:weight=normal:slant=normal" nil "menlokakugo")
  (create-fontset-from-ascii-font "Monaco-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    ;; (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
                    (font-spec :family "M+2VM+IPAG circle" :size 14)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo")))

(require 'color-theme)
;; (load "my-color-theme-window")
;; (my-color-theme-window)
(color-theme-initialize)
(color-theme-hober)

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
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(howm-reminder-today-face ((t (:foreground "orange" :background "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black"))))
 '(egg-header ((t (:weight bold :height 1.1))))
 '(egg-text-4 ((t (:inherit egg-text-base :height 1.4))))
 '(egg-text-base ((((class color) (background dark)) (:inherit fixed :foreground "SteelBlue"))))
 '(egg-text-help ((t (:inherit egg-text-base :height 0.9)))))

;; ;; Wanderlust
;; (require 'mime-setup)
;; (require 'wl)
;; (require 'wl-draft)
;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)

;; ;; w3m
;; (require 'w3m)
;; (require 'mime-w3m)
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

(tool-bar-mode nil)

(when (= emacs-major-version 23)
  (define-key global-map [165] nil)
  (define-key global-map [67109029] nil)
  (define-key global-map [134217893] nil)
  (define-key global-map [201326757] nil)
  (define-key function-key-map [165] [?\\])
  (define-key function-key-map [67109029] [?\C-\\])
  (define-key function-key-map [134217893] [?\M-\\])
  (define-key function-key-map [201326757] [?\C-\M-\\]))

(provide 'init_mac)
;; init_mac.el ends here
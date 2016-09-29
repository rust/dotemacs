;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_x-window.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq fontname (cond
                ((string-match "ThinkpadX220" system-name)
                 "M+2VM+IPAG circle-8")))
(add-to-list 'default-frame-alist (cons 'font fontname))
(set-default-font fontname)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  (cons fontname "unicode-bmp"))

(require 'solarized-light-theme)

;; transparent
(add-to-list 'default-frame-alist '(alpha . 100))
(set-frame-parameter nil 'alpha 100)
(setq frame-alpha-lower-limit 95)

(setq hl-line-face 'underline)
(global-hl-line-mode)

;; Cookie
(setq w3m-use-cookies t)
;; favicon cache
(setq w3m-favicon-cache-expire-wait nil)

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

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

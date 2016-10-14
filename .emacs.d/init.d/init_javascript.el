;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_javascript.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for javascript

(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-mode))

(add-hook 'jsx-mode-hook
          (lambda ()
            (setq jsx-indent-level 2)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; Set 2 spaces tab
(setq-default js2-basic-offset 2)

(provide 'init_javascript)
;; init_javascript.el ends here

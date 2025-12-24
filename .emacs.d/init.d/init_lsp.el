;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_lsp.el

;; Copyright (C) 2025 Shinichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp

(use-package lsp-mode
  :ensure t
  :hook
  ((ruby-mode . lsp-deferred)
   (ruby-ts-mode . lsp-deferred)
   )
  :init
  (setq lsp-ruby-lsp-use-bundler t))

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(provide 'init_lsp)
;; init_lsp.el ends here

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

(provide 'init_lsp)
;; init_lsp.el ends here

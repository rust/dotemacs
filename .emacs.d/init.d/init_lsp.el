;;; init_lsp.el --- LSP設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shinichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Language Server Protocolの設定。
;; 各言語のLSPサーバー設定とlsp-modeの設定を含む。

;;; Code:

;; lsp

(use-package lsp-mode
  :ensure t
  :defer t
  :hook
  ((ruby-mode . lsp-deferred)
   (ruby-ts-mode . lsp-deferred)
   )
  :init
  (setq lsp-ruby-lsp-use-bundler t)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () '("mise" "x" "--" "ruby-lsp")))
    :major-modes '(ruby-mode ruby-ts-mode)
    :server-id 'ruby-lsp-mise)))

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(provide 'init_lsp)
;; init_lsp.el ends here

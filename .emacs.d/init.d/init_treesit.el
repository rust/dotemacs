;;; init_treesit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; init_treesit.el

;; Copyright (C) 2025 Shin-ichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for treesit

(use-package treesit-auto
  :ensure t
  :init
  (require 'treesit-auto)
  (global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t))

(use-package tree-sitter
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(provide 'init_treesit)
;;; init_treesit.el ends here

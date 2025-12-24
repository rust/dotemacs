;;; init_progs.el --- プログラミング言語設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Shin-ichiro OGAWA
;; Author: Shin-ichiro OGAWA <rust@stnard.jp>

;;; Commentary:

;; 各種プログラミング言語の設定。
;; Haskell、Rust、Goなどのモード設定を含む。

;;; Code:

;; misc

;; haskell-mode
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)))
;;(use-package haskell-cabal)

;; go-mode
(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'exec-path (expand-file-name "~/Works/golang/bin"))
  (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/nsf/gocode/emacs"))
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; % go get -u github.com/rogpeppe/godef
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)
                            (setq tab-width 4))))
;; % go get -u github.com/nsf/gocode
(use-package go-autocomplete
  :ensure t
  :defer t)

;; scala-mode
(use-package scala-mode
  :ensure t)

;; kotlin-mode
(use-package kotlin-mode
  :ensure t)

;; elixir-mode
(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :config
  (setq alchemist-key-command-prefix (kbd "C-c a"))
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup))

;; Gauche
(use-package scheme)

;; PHP
(use-package php-mode
  :ensure t
  :mode
  ("\\.php" . php-mode))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; Java/Groovy
(use-package groovy-mode
  :ensure t
  :config
  (add-hook 'groovy-mode-hook '(lambda ()
                                 (c-set-offset 'label 4)))
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

(provide 'init_progs)
;; init_progs.el ends here

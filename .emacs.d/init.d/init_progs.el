;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_progs.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; haskell-mode
(use-package haskell-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode)))
;;(use-package haskell-cabal)

;; go-mode
(use-package go-mode
  :defer t
  :config
  (add-to-list 'exec-path (expand-file-name "~/Works/golang/bin"))
  (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/nsf/gocode/emacs"))
  (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/dougm/goflymake"))
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; % go get -u github.com/rogpeppe/godef
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)
                            (setq tab-width 4))))
;; % go get -u github.com/nsf/gocode
(use-package go-autocomplete :defer t)
;; % go get -u github.com/dougm/goflymake
(use-package go-flymake :defer t)

;; scala-mode
(use-package ensime
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
(use-package sbt-mode)
(use-package scala-mode)

;; kotlin-mode
(use-package kotlin-mode)

;; elixir-mode
(use-package erlang)
(use-package elixir-mode
  :config
  (defun my-elixir-do-end-close-action (id action context)
    (when (eq action 'insert)
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode)))
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "->" "end"
                   :when '(("RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert)))
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert))))
(use-package alchemist
  :config
  (setq alchemist-key-command-prefix (kbd "C-c a"))
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup))

;; Gauche
(use-package scheme-mode
  :commands run-scheme
  :defer t
  :bind (("C-c s" . scheme-other-window))
  :config
  (modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))
  (cond ((or mac-p ns-p)
         (setq scheme-program-name "/opt/local/bin/gosh -i"))
        (linux-p
         (setq scheme-program-name "gosh -i")))
  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)))

;; PHP
(use-package php-mode
  :ensure t
  :mode
  ("\\.php" . pho-mode))

;; Rust
(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; Java/Groovy
(use-package groovy-mode
  :config
  (add-hook 'groovy-mode-hook '(lambda ()
                                 (c-set-offset 'label 4)))
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))


;; Python
(use-package python-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  (setq jedi:complete-on-dot t)
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-enable-on-save)
  (require 'flymake-python-pyflakes)
  (flymake-python-pyflakes-load))

(provide 'init_progs)
;; init_progs.el ends here
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

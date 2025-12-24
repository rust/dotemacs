;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_progs.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :ensure t
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
  :ensure t
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
  (cond (mac-p (setq scheme-program-name "/opt/local/bin/gosh -i"))
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

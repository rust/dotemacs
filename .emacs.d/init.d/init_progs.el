;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_progs.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; haskell-mode
(use-package ghc
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))

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
                            (local-set-key (kbd "M-.") 'godef-jump))))
;; % go get -u github.com/nsf/gocode
(use-package go-autocomplete :defer t)
;; % go get -u github.com/dougm/goflymake
(use-package go-flymake :defer t)

;; scala-mode
(use-package ensime
  :ensure t
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
(use-package sbt-mode
  :pin melpa)
(use-package scala-mode
  :pin melpa)

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
  :config
  (require 'cl)
  (add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ) )
               (yas-global-mode 1)
               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
               ))
  )

;; Rust
(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(provide 'init_progs)
;; init_progs.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_progs.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; haskell-mode
(use-package ghc-mod
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))

;; go-mode
(use-package go-mode
  :config
  (add-to-list 'exec-path (expand-file-name "~/Works/golang/bin"))
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; % go get -u github.com/rogpeppe/godef
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "M-.") 'godef-jump)))
  ;; % go get -u github.com/nsf/gocode
  (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/nsf/gocode/emacs"))
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  ;; % go get -u github.com/dougm/goflymake
  (add-to-list 'load-path (expand-file-name "~/Works/golang/src/github.com/dougm/goflymake"))
  (require 'go-flymake))

(provide 'init_progs)
;; init_progs.el ends here

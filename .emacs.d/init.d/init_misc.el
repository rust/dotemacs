;;; init_misc.el --- その他の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Shin-ichiro OGAWA
;; Author: Shin-ichiro OGAWA <rust@stnard.jp>

;;; Commentary:

;; 各種言語モードやツールの設定。
;; Haskell、CSS、JSON、yasnippet、elscreen、auto-completeなどを含む。

;;; Code:

;; misc

(desktop-save-mode t)
(setq desktop-auto-save-timeout 60)
(add-hook 'kill-emacs-hook
          (lambda ()
            (desktop-save (expand-file-name "~/.emacs.d/") t)))

;; haskell-mode
(use-package
  haskell-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hs$"    . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$"   . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))

;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;; css-mode
(use-package css-mode
  :config
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (setq css-indent-offset 2))

;;; Interactively Do Things
(use-package ido
  :config
  (ido-mode t))

;; json-mode
(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2))))

;; jsonnet-mode
(use-package jsonnet-mode
  :ensure t
  :config
  (add-hook 'jsonnet-mode-hook
            '(lambda ()
               (setq comment-start "#")))
  (add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode))
  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode)))

;; anzu
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1)

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-use-mimego t)
   '(anzu-replace-to-string-separator " => "))
  (bind-keys ("M-%"   . anzu-query-replace)
             ("C-M-%" . anzu-query-replace-regexp)))

;; yasnippet
(use-package yasnippet
  :ensure t
  :mode (("\\.yasnippet$" . snippet-mode))
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/elisp/yasnippet/snippets"
          ))

  ;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
  ;; (setqだとtermなどで干渉問題ありでした)
  (custom-set-variables '(yas-trigger-key "TAB"))

  ;; 既存スニペットを挿入する
  (define-key yas-minor-mode-map (kbd "C-x s i") 'yas-insert-snippet)
  ;; 新規スニペットを作成するバッファを用意する
  (define-key yas-minor-mode-map (kbd "C-x s n") 'yas-new-snippet)
  ;; 既存スニペットを閲覧・編集する
  (define-key yas-minor-mode-map (kbd "C-x s v") 'yas-visit-snippet-file))

;; elscreen
(use-package elscreen
  :ensure t
  ;; :bind (("C-z SPC"           . elscreen-next)
  ;;        ("C-z DEL"           . elscreen-previous)
  ;;        ;; compatibility for MacOS X
  ;;        ("M-t"               . elscreen-create)
  ;;        ("M-T"               . elscreen-clone)
  ;;        ("M-}"               . elscreen-next)
  ;;        ("M-{"               . elscreen-previous)
  ;;        ([(s t)]             . elscreen-create)
  ;;        ;; (global-set-key [(s w)] . elscreen-kill)
  ;;        ([(s })]             . elscreen-next)
  ;;        ([(s {)]             . elscreen-previous)
  ;;        ([(C-tab)]           . elscreen-next)
  ;;        ([(C-S-iso-lefttab)] . elscreen-previous)
  :config
  ;; (require 'elscreen-gf)
  ;; (require 'elscreen-w3m)
  (elscreen-start)
  ;; prefix-setting
  (elscreen-set-prefix-key "\C-z")
  (global-set-key (kbd "C-z SPC") 'elscreen-next)
  (global-set-key (kbd "C-z DEL") 'elscreen-previous)
  ;; compatibility for MacOS X
  (global-set-key "\M-t" 'elscreen-create)
  (global-set-key "\M-T" 'elscreen-clone)
  (global-set-key "\M-}" 'elscreen-next)
  (global-set-key "\M-{" 'elscreen-previous)
  (global-set-key [(s t)] 'elscreen-create)
  ;; (global-set-key [(s w)] 'elscreen-kill)
  (global-set-key [(s })] 'elscreen-next)
  (global-set-key [(s {)] 'elscreen-previous)
  (global-set-key [(C-tab)] 'elscreen-next)
  (global-set-key [(C-S-iso-lefttab)] 'elscreen-previous))

;; autocomplete
(use-package auto-complete
  :commands auto-complete-config
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/dict")
  (ac-config-default)
  ;; for global
  (global-auto-complete-mode t)
  (define-key ac-completing-map (kbd "M-n") 'ac-next)
  (define-key ac-completing-map (kbd "M-p") 'ac-previous)
  (setq ac-dwim t)
  ;; sources
  (setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hool (lambda () (add-to-list 'ac-sources 'ac-source-symbold t)))
  ;; automatic completion
  (setq ac-auto-start 5)
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (setq ac-quick-help-prefer-x t))

;; multi-term
(use-package multi-term
  :ensure t
  :bind (("C-c n" . multi-term-next)
         ("C-c p" . multi-term-prev))
  :config
  (setq multi-term-program shell-file-name)

  (add-hook 'term-mode-hook
            '(lambda ()
               ;; C-h を term 内文字削除にする
               (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
               ;; C-y を term 内ペーストにする
               (define-key term-raw-map (kbd "C-y") 'term-paste)
               ))

  (global-set-key (kbd "C-c t") '(lambda ()
                                   (interactive)
                                   (if (get-buffer "*terminal<1>*")
                                       (switch-to-buffer "*terminal<1>*")
                                     (multi-term)))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'init_misc)
;; init_misc.el ends here

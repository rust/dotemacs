;;; init_misc.el --- その他の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; 各種言語モードやツールの設定。
;; CSS、JSON、yasnippet、elscreen、multi-termなどを含む。
;; haskell-mode は init_progs.el に集約。
;; 補完は company（init_lsp.el）に一本化。ファイル選択は vertico（init_finder.el）に一本化。

;;; Code:

;; misc

;; (desktop-save-mode t)
;; (setq desktop-auto-save-timeout 60)
;; (add-hook 'kill-emacs-hook
;;           (lambda ()
;;             (desktop-save (expand-file-name "~/.emacs.d/") t)))

;; haskell-mode は init_progs.el に集約

;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;; css-mode
(use-package css-mode
  :ensure t
  :defer t
  :config
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (setq css-indent-offset 2))

;; ido は vertico（init_finder.el）に置き換え済みのため削除

;; json-mode
(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2))))

;; jsonnet-mode
(use-package jsonnet-mode
  :ensure t
  :defer t
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

;; auto-complete は company（init_lsp.el）に置き換え済みのため削除

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
  :defer t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package nix-mode
  :ensure t
  :defer t)

(provide 'init_misc)
;; init_misc.el ends here

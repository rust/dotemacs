;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_misc.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; for TeX
(use-package yatex
  :mode (("\\.tex$" . yatex-mode))
  :config
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-use-AMS-LaTeX t))

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

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
  :config
  (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2))))

;; jsonnet-mode
(use-package jsonnet-mode
  :config
  (add-hook 'jsonnet-mode-hook
            '(lambda ()
               (setq comment-start "#")))
  (add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode))
  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode)))

;; anzu
(use-package anzu
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

;; migemo
(use-package migemo
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Set your installed path
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  (helm-migemo-mode 1))

;; yasnippet
(use-package yasnippet
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
  (define-key yas-minor-mode-map (kbd "C-x s v") 'yas-visit-snippet-file)

  ;; helm interface
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key helm-command-map (kbd "y") 'yas/insert-snippet)))

;; elscreen
(use-package elscreen
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
  ;;        ("C-z C-a"           . helm-elscreen))
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
  (global-set-key [(C-S-iso-lefttab)] 'elscreen-previous)
  (global-set-key (kbd "C-z C-a") 'helm-elscreen))

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

;; session.el
;;   kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
(use-package session
  :config
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1))

;; multi-term
(use-package multi-term
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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "<menu>") 'counsel-M-x))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(provide 'init_misc)
;; init_misc.el ends here

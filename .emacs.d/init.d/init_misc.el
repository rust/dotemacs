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

;; tab-bar-mode（elscreen の代替、Emacs 27+ 組み込み）
(tab-bar-mode 1)
(let ((map (make-sparse-keymap)))
  ;; 作成・複製・削除
  (define-key map (kbd "c")   'tab-new)
  (define-key map (kbd "C-c") 'tab-new)
  (define-key map (kbd "C")   'tab-duplicate)
  (define-key map (kbd "k")   'tab-close)
  (define-key map (kbd "C-k") 'tab-close)
  (define-key map (kbd "K")   'tab-close-other)
  ;; 移動
  (define-key map (kbd "n")   'tab-next)
  (define-key map (kbd "C-n") 'tab-next)
  (define-key map (kbd "SPC") 'tab-next)
  (define-key map (kbd "p")   'tab-previous)
  (define-key map (kbd "C-p") 'tab-previous)
  (define-key map (kbd "DEL") 'tab-previous)
  (define-key map (kbd "a")   'tab-last)
  (define-key map (kbd "C-a") 'tab-last)
  ;; 番号指定ジャンプ（1〜9）
  (dotimes (i 9)
    (define-key map (kbd (number-to-string (1+ i)))
      `(lambda () (interactive) (tab-bar-select-tab ,(1+ i)))))
  ;; 名前で選択・リネーム
  (define-key map (kbd "\"")  'tab-switch)
  (define-key map (kbd "A")   'tab-rename)
  ;; ファイルを新タブで開く
  (define-key map (kbd "C-f") (lambda () (interactive) (tab-new) (call-interactively 'find-file)))
  ;; タブ移動
  (define-key map (kbd "C-s") 'tab-move)
  ;; tab-bar 表示トグル
  (define-key map (kbd "T")   'tab-bar-mode)
  (global-set-key (kbd "C-z") map))
;; Mac 互換キーバインド
(global-set-key "\M-t"             'tab-new)
(global-set-key "\M-T"             'tab-duplicate)
(global-set-key "\M-}"             'tab-next)
(global-set-key "\M-{"             'tab-previous)
(global-set-key [(s t)]            'tab-new)
(global-set-key [(s })]            'tab-next)
(global-set-key [(s {)]            'tab-previous)
(global-set-key [(C-tab)]          'tab-next)
(global-set-key [(C-S-iso-lefttab)] 'tab-previous)

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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package nix-mode
  :ensure t
  :defer t)

(provide 'init_misc)
;; init_misc.el ends here

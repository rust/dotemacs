;;; init_templates.el --- テンプレート言語の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; HAMLやSlimなどのテンプレート言語の設定。

;;; Code:

;; haml
(use-package haml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
  (add-hook 'haml-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (define-key haml-mode-map "\C-m" 'newline-and-indent)))
  (add-hook 'haml-mode-hook
            (lambda ()
              (set (make-local-variable 'electric-indent-functions)
                   (list (lambda (arg) 'no-indent))))))

;; sass
(use-package sass-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (add-hook 'sass-mode-hook
            (lambda ()
              (set (make-local-variable 'electric-indent-functions)
                   (list (lambda (arg) 'no-indent))))))

;; slim
(use-package slim-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))
  (add-hook 'slim-mode-hook
            (lambda ()
              (set (make-local-variable 'electric-indent-functions)
                   (list (lambda (arg) 'no-indent))))))

;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'php-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; electric-pair-mode（smartparens の代替、Emacs 組み込み）
;; 括弧・クォートの自動ペアリング。smartparens より軽量でファイルオープンが高速。
(electric-pair-mode 1)

(provide 'init_templates)
;; init_haml.el ends here

;;; init_markdown.el --- Markdown設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Markdownファイル編集のための設定。

;;; Code:

;; markdown-mode
(use-package visual-fill-column
  :ensure t)

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-hide-markup nil)
  :config
  (defun my/markdown-writing-mode ()
    "Optimize markdown buffers for long-form writing."
    (visual-line-mode 1)
    (setq-local truncate-lines nil
                word-wrap t
                visual-fill-column-width 100
                visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  (add-hook 'markdown-mode-hook #'my/markdown-writing-mode)
  (add-hook 'gfm-mode-hook #'my/markdown-writing-mode)
  (defun my/apply-markdown-glow-faces (&rest _)
    "Apply glow-like colors for markdown buffers."
    (set-face-attribute 'markdown-header-face-1 nil :foreground "#268bd2" :weight 'bold :height 1.45)
    (set-face-attribute 'markdown-header-face-2 nil :foreground "#2aa198" :weight 'bold :height 1.32)
    (set-face-attribute 'markdown-header-face-3 nil :foreground "#859900" :weight 'bold :height 1.22)
    (set-face-attribute 'markdown-header-face-4 nil :foreground "#b58900" :weight 'bold :height 1.14)
    (set-face-attribute 'markdown-header-face-5 nil :foreground "#cb4b16" :weight 'bold :height 1.08)
    (set-face-attribute 'markdown-header-face-6 nil :foreground "#6c71c4" :weight 'bold)
    (set-face-attribute 'markdown-markup-face nil :foreground "#93a1a1")
    (set-face-attribute 'markdown-header-delimiter-face nil :foreground "#268bd2" :weight 'bold)
    (set-face-attribute 'markdown-list-face nil :foreground "#2aa198" :weight 'bold)
    (set-face-attribute 'markdown-blockquote-face nil :foreground "#6c71c4" :slant 'italic)
    (set-face-attribute 'markdown-bold-face nil :foreground "#073642" :weight 'bold)
    (set-face-attribute 'markdown-italic-face nil :foreground "#d33682" :slant 'italic)
    (set-face-attribute 'markdown-inline-code-face nil :foreground "#cb4b16" :background "#eee8d5")
    (set-face-attribute 'markdown-code-face nil :foreground "#859900" :background "#eee8d5")
    (set-face-attribute 'markdown-pre-face nil :foreground "#586e75" :background "#eee8d5")
    (set-face-attribute 'markdown-language-keyword-face nil :foreground "#b58900" :weight 'bold)
    (set-face-attribute 'markdown-language-info-face nil :foreground "#2aa198")
    (set-face-attribute 'markdown-link-face nil :foreground "#268bd2" :underline t)
    (set-face-attribute 'markdown-url-face nil :foreground "#2aa198" :underline t)
    (set-face-attribute 'markdown-reference-face nil :foreground "#6c71c4")
    (set-face-attribute 'markdown-table-face nil :foreground "#b58900")
    (set-face-attribute 'markdown-footnote-marker-face nil :foreground "#d33682" :weight 'bold)
    (set-face-attribute 'markdown-footnote-text-face nil :foreground "#6c71c4")
    (set-face-attribute 'markdown-gfm-checkbox-face nil :foreground "#859900" :weight 'bold))
  (my/apply-markdown-glow-faces)
  (advice-add 'load-theme :after #'my/apply-markdown-glow-faces))

(provide 'init_markdown)
;; init_markdown.el ends here

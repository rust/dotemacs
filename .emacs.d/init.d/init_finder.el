;;; init_finder.el --- ファイル検索・選択の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Vertico + Orderless + Marginalia + Consult によるミニバッファ補完スタック。
;; ido の代替として以下を提供する：
;;   vertico   - 縦リスト表示の補完 UI
;;   orderless - 部分一致・スペース区切りあいまい補完（ido 感覚の操作）
;;   marginalia - 補完候補にアノテーション（説明・モードなど）を付加
;;   consult   - バッファ切替・ファイル検索など強化コマンド群

;;; Code:

(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
         completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :config
  (require 'corfu-auto)
  (require 'corfu-history)
  (setq corfu-auto-prefix 2
        corfu-auto-delay 0.15)
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3)
  (global-corfu-mode 1)
  (corfu-history-mode 1))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

(use-package recentf
  :ensure nil
  :demand t
  :config
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 'mode
        recentf-filename-handlers nil)
  (recentf-mode 1))

(use-package consult
  :ensure t
  :demand t
  :config
  (defvar my/consult-source-recent-file
    (list :name "Recent File"
          :narrow ?r
          :category 'file
          :face 'consult-file
          :history 'file-name-history
          :state #'consult--file-state
          :new #'consult--file-action
          :enabled (lambda () recentf-mode)
          :items
          (lambda ()
            (let ((ht (consult--buffer-file-hash))
                  items)
              (dolist (file (bound-and-true-p recentf-list) (nreverse items))
                (unless (eq (aref file 0) ?/)
                  (let (file-name-handler-alist)
                    (setq file (expand-file-name file))))
                (unless (gethash file ht)
                  (push (consult--fast-abbreviate-file-name file) items)))))))
  (setq consult-buffer-sources
        '(consult-source-buffer
          consult-source-hidden-buffer
          consult-source-modified-buffer
          consult-source-other-buffer
          my/consult-source-recent-file
          consult-source-buffer-register
          consult-source-file-register
          consult-source-bookmark
          consult-source-project-buffer-hidden
          consult-source-project-recent-file-hidden
          consult-source-project-root-hidden))
  :bind
  (("C-x b"   . consult-buffer)        ; ido-switch-buffer の代替
    ("C-x C-f" . find-file)             ; 標準のまま（vertico が補完）
    ("M-y"     . consult-yank-pop)      ; kill-ring から貼り付け
   ("M-s l"   . consult-line)          ; バッファ内一覧検索（isearch 中からも呼べる）
   ("C-c g"   . consult-ripgrep))      ; プロジェクト全体 grep
  :bind
  (:map isearch-mode-map
   ("M-s l"   . consult-line)))        ; isearch 中に一覧検索へ切り替え

(use-package project
  :ensure nil
  :bind
  (("C-c p f" . project-find-file)
   ("C-c p b" . project-switch-to-buffer)
   ("C-c p p" . project-switch-project)
   ("C-c p g" . project-find-regexp)))

(provide 'init_finder)
;; init_finder.el ends here

;;; init_ai.el --- AI支援ツール設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shinichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; AI支援ツールの設定。
;; Copilotなどのコード補完AIツールの設定を含む。

;;; Code:

;; AI

(use-package copilot
  :ensure t
  :config
  (setq copilot-indent-offset-warning-disable t)
  ;; 起動時に preload して、ファイルを開いた時点で即座に有効化する
  (defun my/enable-copilot-on-find-file ()
    "Enable Copilot immediately in file-visiting buffers."
    (copilot-mode 1))
  (add-hook 'find-file-hook #'my/enable-copilot-on-find-file)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'init_ai)
;; init_ai.el ends here

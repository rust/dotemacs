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
  (add-hook 'prog-mode-hook 'copilot-mode)
  (setq copilot-indent-offset-warning-disable t)

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'init_ai)
;; init_ai.el ends here

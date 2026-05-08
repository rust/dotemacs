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
  :defer t
  :config
  (setq copilot-indent-offset-warning-disable t)
  ;; ファイルオープン時のブロッキングを防ぐため idle timer 経由で起動
  (add-hook 'find-file-hook
            (lambda () (run-with-idle-timer 1.0 nil #'copilot-mode 1)))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'init_ai)
;; init_ai.el ends here

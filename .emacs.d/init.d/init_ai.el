;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ai.el

;; Copyright (C) 2025 Shinichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

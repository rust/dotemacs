;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ai.el

;; Copyright (C) 2025 Shinichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI

(use-package copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(provide 'init_ai)
;; init_ai.el ends here

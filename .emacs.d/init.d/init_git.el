;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_git.el

;; Copyright (C) 2013 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

;; magit.el
(setq magit-last-seen-setup-instructions "1.4.0")

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  :config
  (add-hook 'prog-mode-hook 'git-gutter-mode))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(provide 'init_git)
;; init_git.el ends here

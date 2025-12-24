;;; init_git.el --- Git設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Gitツールの設定。
;; Magitなどのgit連携ツールの設定を含む。

;;; Code:

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

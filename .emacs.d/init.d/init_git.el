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
  :ensure t
  :defer t
  :custom
  (magit-git-executable (cond (mac-p "/opt/homebrew/bin/git")
                              (t "/usr/bin/git"))))

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:update-interval 2)
  ;; 起動時に preload して、ファイルを開いた時点で即座に有効化する
  (defun my/enable-git-gutter-on-find-file ()
    "Enable git-gutter immediately in file-visiting buffers."
    (git-gutter-mode 1))
  (add-hook 'find-file-hook #'my/enable-git-gutter-on-find-file))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(provide 'init_git)
;; init_git.el ends here

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
  ;; ファイルオープン時の即時 git diff を防ぐため idle timer 経由で起動
  (add-hook 'find-file-hook
            (lambda () (run-with-idle-timer 0.5 nil #'git-gutter-mode 1))))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(provide 'init_git)
;; init_git.el ends here

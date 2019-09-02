;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_git.el

;; Copyright (C) 2013 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

;; magit.el
(setq magit-last-seen-setup-instructions "1.4.0")

(use-package magit
  :after (ivy)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter+
  :init (global-git-gutter+-mode)
  :config
  (add-hook 'ruby-mode-hook 'git-gutter-mode)
  (add-hook 'enh-ruby-mode-hook 'git-gutter-mode)
  (add-hook 'coffee-mode-hook 'git-gutter-mode)
  (add-hook 'haml-mode-hook 'git-gutter-mode)
  (add-hook 'rhtml-mode-hook 'git-gutter-mode)
  (add-hook 'sass-mode-hook 'git-gutter-mode)
  (add-hook 'scss-mode-hook 'git-gutter-mode))

(provide 'init_git)
;; init_git.el ends here

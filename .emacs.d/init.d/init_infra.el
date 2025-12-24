;;; init_infra.el --- インフラ関連設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; インフラ系ファイルの設定。
;; シェルスクリプト、設定ファイル、Dockerfileなどのモード設定を含む。

;;; Code:

;; infra

;; script-mode
(add-to-list 'auto-mode-alist '("\\.zsh" . shell-script-mode))

;; conf-mode
(add-to-list 'auto-mode-alist '("ssh/config"    . conf-mode))
(add-to-list 'auto-mode-alist '("\\*ssh_config" . conf-mode))

;; docker
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :ensure t)

;; Nginx
(use-package nginx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("nginx\\(.*\\).conf[^/]*$" . nginx-mode))
  (add-hook 'nginx-mode-hook (lambda ()
                               (setq indent-tabs-mode nil)
                               (setq c-basic-offset 4)
                               (setq tab-width 4))))

(use-package terraform-mode
  :ensure t)
(use-package hcl-mode
  :ensure t)
(use-package toml-mode
  :ensure t)
(use-package vimrc-mode
  :ensure t)

(provide 'init_infra)
;; init_infra.el ends here

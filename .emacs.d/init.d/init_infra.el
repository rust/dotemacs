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
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; docker-compose: yaml-ts-mode で代替（docker-compose-mode は yaml-mode 依存で削除済み）
(add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.ya?ml\\'" . yaml-ts-mode))

;; Nginx
(use-package nginx-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("nginx\\(.*\\).conf[^/]*$" . nginx-mode))
  (add-hook 'nginx-mode-hook (lambda ()
                               (setq indent-tabs-mode nil)
                               (setq c-basic-offset 4)
                               (setq tab-width 4))))

(use-package terraform-mode
  :ensure t
  :defer t)
(use-package hcl-mode
  :ensure t
  :defer t)
;; toml-ts-mode は Emacs 29+ 組み込み（toml-mode は削除済み）
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(use-package vimrc-mode
  :ensure t
  :defer t)

(provide 'init_infra)
;; init_infra.el ends here

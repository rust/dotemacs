;;; init.el --- Emacs設定のエントリーポイント  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Emacs初期化ファイル。
;; load-path の設定、パッケージ管理、各種設定モジュールの読み込みを行う。

;;; Code:

;; path-list を load-path へ追加する

;; disalbe Mac Magic File Name
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; LSP runtime tuning: 起動コストよりも編集中の応答性を優先する
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.5
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil)

(defun add-to-load-path (path-list)
  "Add paths to 'load-path"
  (let (path)
    (dolist (path path-list path-list)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
;; init.d と elisp を追加
(add-to-load-path (list "init.d" "elisp"))

;; 環境判別
(defvar mac-p (or (eq window-system 'mac) (eq window-system 'ns)))
(defvar window-p (or mac-p (eq window-system 'x)))

;; use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; インストール失敗時に一度だけキャッシュを更新してリトライ
(defvar my/package-refreshed-on-error nil)
(defun my/package-install-refresh-on-error (orig-fn &rest args)
  (condition-case nil
      (apply orig-fn args)
    (error
     (unless my/package-refreshed-on-error
       (setq my/package-refreshed-on-error t)
       (package-refresh-contents))
     (apply orig-fn args))))
(advice-add 'package-install :around #'my/package-install-refresh-on-error)

(require 'use-package)
(setq use-package-always-ensure t)

;; PATHの設定: exec-path-from-shell でシェル環境を引き継ぐ（手動 dolist を廃止）
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; 共通設定ファイル
(require 'init_main)

;; Color theme and indent guide
(use-package solarized-theme
  :ensure t)

;; 環境依存設定ファイル
(cond
 (window-p (require 'init_window))
 (t (require 'init_terminal-mode)))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(global-font-lock-mode t)

;; restore Mac Magic File Name
(setq file-name-handler-alist my-saved-file-name-handler-alist)

(provide 'init)

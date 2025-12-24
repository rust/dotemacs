;;; init.el --- Emacs設定のエントリーポイント  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shin-ichiro OGAWA <rust@stnard.jp>

;;; Commentary:

;; Emacs初期化ファイル。
;; load-path の設定、パッケージ管理、各種設定モジュールの読み込みを行う。

;;; Code:

;; path-list を load-path へ追加する

(setq debug-on-error t)
(global-tree-sitter-mode)


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

;; PATHの設定 from http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
 ;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Set PATH
(use-package exec-path-from-shell)

;; 共通設定ファイル
(require 'init_main)

(use-package solarized-theme)

;; 環境依存設定ファイル
(cond
 (window-p (require 'init_window))
 (t (require 'init_terminal-mode)))

(provide 'init)

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path-list を load-path へ追加する

(setq debug-on-message "Warning (el-get)")

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

;; packages
(require 'my-package)

;; Set PATH
(exec-path-from-shell-initialize)

;; 共通設定ファイル
(require 'init_main)

;; 環境依存設定ファイル
(cond
 (window-p (require 'init_window))
 (t (require 'init_terminal-mode)))

(provide 'init)

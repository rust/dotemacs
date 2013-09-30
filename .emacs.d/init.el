;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user-emacs-directory for 22.x
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))
;; path-list を load-path へ追加する
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
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar emacs-p (>= emacs-major-version 23))
(defvar mac-p (and (eq window-system 'mac) emacs-p))
(defvar ns-p (and (eq window-system 'ns) emacs-p))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar x-window-p (and (eq window-system 'x) linux-p))
(defvar no-x-p (and (not (eq window-system 'x)) linux-p))
(defvar lubuntu-p (string= system-name "netbook"))
(defvar macbook-air-p (string-match "Macbook-Air" "Shin-ichiro-no-MacBook-Air.local"))

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
              (expand-file-name "~/.rvm/gems/ruby-1.9.3-p194/bin")
              (expand-file-name "~/.rvm/gems/ruby-1.9.3-p194@global/bin")
              (expand-file-name "~/.rvm/rubies/ruby-1.9.3-p194/bin")
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
 ;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; 23と24.1の互換性維持のため
(when (not (boundp 'inhibit-first-line-modes-regexps))
  (defvaralias 'inhibit-first-line-modes-regexps 'inhibit-local-variables-regexps))

;; 共通設定ファイル
(require 'init_main)

;; 環境依存設定ファイル
(cond
 ((or ns-p mac-p) (require 'init_mac))
 (linux-p (require 'init_linux)))

;; ;; 終了時バイトコンパイル
;; (add-hook 'kill-emacs-query-functions
;;          (lambda ()
;;            (if (file-newer-than-file-p (concat user-emacs-directory "init.el") (concat user-emacs-directory "init.elc"))
;;                (byte-compile-file (concat user-emacs-directory "init.el")))
;;            (byte-recompile-directory (concat user-emacs-directory "init.d") 0)
;;            (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
;;            ))

(provide 'init)

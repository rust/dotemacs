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
(defvar mac-p (and (eq window-system 'mac) emacs23-p))
(defvar ns-p (and (eq window-system 'ns) emacs23-p))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar x-window-p (and (eq window-system 'x) linux-p))
(defvar no-x-p (and (not (eq window-system 'x)) linux-p))

;; 共通設定ファイル
(require 'init_main)
;; 環境依存設定ファイル
(cond
 ((or ns-p mac-p) (require 'init_mac))
 (linux-p (require 'init_linux)))

;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p (concat user-emacs-directory "init.el") (concat user-emacs-directory "init.elc"))
                (byte-compile-file (concat user-emacs-directory "init.el")))
            (byte-recompile-directory (concat user-emacs-directory "local-lisp") 0)
            (byte-recompile-directory (concat user-emacs-directory "private") 0)
            (byte-recompile-directory (concat user-emacs-directory "site-start.d") 0)
            ))

;; 起動時間計測 目標は常に 3000ms 圏内(dump-emacs すれば可能だがしてない)
(when emacs23-p
  (defun message-startup-time ()
    (message "Emacs loaded in %dms"
             (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
                   (+ (third before-init-time) (* 1000000 (second before-init-time))))
                1000)))
  (add-hook 'after-init-hook 'message-startup-time))

(provide 'init)
;; init.el ends here
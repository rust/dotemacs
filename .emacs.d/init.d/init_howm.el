;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_howm.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; howm

(setq howm-view-title-header "#")
(setq howm-view-title-regexp-grep "^\# +[^2]")
(setq howm-view-title-regexp "^\\#\\( +\\([^2].*\\)\\|\\)$")

(require 'howm)

(global-set-key "\C-c,," 'howm-menu)
(global-set-key "\C-c,a" 'howm-list-all)
(global-set-key "\C-c,c" 'howm-create)

(setq howm-directory "~/Dropbox/howm")          ;; memoの場所
(setq howm-view-summary-persistent nil) ;; ファイルを開く際に一覧を消す
(setq howm-list-recent-title t)         ;; 最近のメモ時にタイトル表示
(setq howm-list-all-title t)            ;; 一覧時にタイトル表示
(setq howm-menu-expiry-hours 2)         ;; ２時間キャッシュ
;;(add-hook 'howm-mode-on-hook 'auto-fill-mode)
(setq howm-view-summary-persistent nil)
;;
(setq howm-menu-schedule-days-before 10) ;; 10日前から
(setq howm-menu-schedule-days 3)         ;; 3日後まで
;;
(setq howm-refresh-after-save t)
(setq howm-menu-refresh-after-save t)
(setq howm-menu-recent-num 30)
(setq howm-menu-todo-num 10)
(setq howm-view-keep-one-window t)
(setq howm-normalizer 'howm-view-sort-by-reverse-date)
(setq howm-list-prefer-word nil)
(add-to-list 'auto-mode-alist '("\\.howm$" . howm-mode))

(defun howm-my-save-and-kill-buffer ()
  (interactive)
  (save-buffer)
  (kill-buffer nil))
(define-key howm-mode-map "\C-c\C-c" 'howm-my-save-and-kill-buffer)

(setq howm-file-name-format "%Y%m%d-%H%M%S.md")

(provide 'init_howm)
;; init_howm.el ends here

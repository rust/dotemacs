;;; init_rails.el --- Rails開発環境設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Ruby on Rails開発のための設定。

;;; Code:

;; for rails
;;;; .rhtml は web-mode で処理（rhtml-mode は削除済み）
;;;; .erb は init_javascript.el の web-mode 設定でカバー済み
(add-to-list 'auto-mode-alist '("\\.rhtml$" . web-mode))

(provide 'init_rails)
;; init_rails.el ends here

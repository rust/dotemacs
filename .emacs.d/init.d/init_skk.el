;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_skk.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKK
(add-to-list 'load-path "~/local/emacs/elisp/skk")
(require 'skk-autoloads)
(setq skk-preload t)

;; C-j の機能を別のキーに割り当て
(global-set-key (kbd "C-m") 'newline-and-indent)
;; C-\ でも SKK に切り替えられるように設定
(setq default-input-method "japanese-skk")
;; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)
;; 漢字登録時、送り仮名が厳密に正しいかをチェック
(setq skk-check-okurigana-on-touroku t)

(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

;; i-search
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

;; Key setting
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

;; for mac
(setq mac-pass-control-to-system nil)

;; for .skk

;; メッセージを日本語で通知する
(setq skk-japanese-message-and-error t)

;; ▼モードで Enter キーを押したとき確定するのみ。改行しない。
(setq skk-egg-like-newline t)

;; ▼モードで BS を押したときには確定しないで前候補を表示する
(setq skk-delete-implies-kakutei nil)

;; 対応する閉括弧を自動的に挿入する
(setq skk-auto-insert-paren t)

;; 文章系のバッファを開いた時には自動的に英数モード(「SKK」モード)に入る
(let ((function #'(lambda ()
                    (require 'skk)
                    (skk-latin-mode-on))))
  (dolist (hook '(find-file-hooks
                  ;; ...
                  mail-setup-hook
                  message-setup-hook))
    (add-hook hook function)))

;;;; かなモードの入力で (モード変更を行なわずに) 長音(ー)を
;;;; ASCII 数字の直後では `-' に、全角数字の直後では `－' にしたい。
;;(setq skk-rom-kana-rule-list
;;      (cons '("-" nil skk-hyphen)
;;            skk-rom-kana-rule-list))
;;(defun skk-hyphen (arg)
;;  (let ((c (char-before (point))))
;;    (cond ((null c) "ー")
;;          ((and (<= ?0 c) (>= ?9 c)) "-")
;;          ((and (<= ?０ c) (>= ?９ c)) "－")
;;          (t "ー"))))
;;
;;;; かなモードの入力でモード変更を行わずに、数字入力中の
;;;; 小数点 (.) およびカンマ (,) 入力を実現する。
;;;; (例) かなモードのまま 1.23 や 1,234,567 などの記述を行える。
;;;; period
;;(setq skk-rom-kana-rule-list
;;      (cons '("." nil skk-period)
;;            skk-rom-kana-rule-list))
;;(defun skk-period (arg)
;;  (let ((c (char-before (point))))
;;    (cond ((null c) "。")
;;          ((and (<= ?0 c) (>= ?9 c)) ".")
;;          ((and (<= ?０ c) (>= ?９ c)) "．")
;;          (t "。"))))
;;
;;;; comma
;;(setq skk-rom-kana-rule-list
;;      (cons '("," nil skk-comma)
;;            skk-rom-kana-rule-list))
;;(defun skk-comma (arg)
;;  (let ((c (char-before (point))))
;;    (cond ((null c) "、")
;;          ((and (<= ?0 c) (>= ?9 c)) ",")
;;          ((and (<= ?０ c) (>= ?９ c)) "，")
;;          (t "、"))))

(provide 'init_skk)
;; init_skk.el ends here

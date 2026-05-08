;;; init_window.el --- ウィンドウシステム設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shinichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; ウィンドウシステム環境の設定。
;; フレーム設定、ツールバー、フォントなどの設定を含む。

;;; Code:

;; initialization for Window System

;; hide menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; Font: Moralerspace
(add-to-list 'default-frame-alist '(font . "Moralerspace Neon HWJPDOC-10"))

;; transparent
(add-to-list 'default-frame-alist '(alpha . 100))
(set-frame-parameter nil 'alpha 100)
(setq frame-alpha-lower-limit 95)

(setq hl-line-face 'underline)
(global-hl-line-mode)

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(cond
 (mac-p (require 'init_mac)))

(load-theme 'solarized-light t)

;; tab-bar: アクティブタブが浮き上がるように solarized-light に合わせて調整
;; （テーマのデフォルトはアクティブ・非アクティブの区別がつきにくいため上書き）
(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar nil
                      :background "#eee8d5"   ; base2: バー全体はやや暗め
                      :foreground "#657b83")  ; base00
  (set-face-attribute 'tab-bar-tab nil
                      :background "#fdf6e3"   ; base3: アクティブタブはコンテンツ背景と同色で浮き上がる
                      :foreground "#073642"   ; base02: 暗く太字で強調
                      :weight 'bold
                      :box '(:line-width 1 :color "#268bd2")) ; blue でアクティブを明示
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background "#eee8d5"   ; base2: バーに溶け込む
                      :foreground "#93a1a1"   ; base1: 薄く表示
                      :weight 'normal
                      :box nil))

(provide 'init_window)
;; init_window.el ends here

;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_setting.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定

;; 大文字変換を無効化
(put 'upcase-region 'disabled nil)
;; 日本語
(set-language-environment 'Japanese)
;; (set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
;; Startup message を非表示
(setq inhibit-startup-message t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; フレームフォーマット
(setq frame-title-format (concat  "%b - emacs@" (system-name)))
;; default to unified diffs
(setq diff-switches "-u")
;;
(temp-buffer-resize-mode 1)
(line-number-mode t)
(column-number-mode t)
;;
(show-paren-mode t) ; 対応する括弧を光らせる。
(transient-mark-mode t) ; 選択部分のハイライト
;; TAB を 2文字分に
(setq-default tab-width 2)
(setq tab-width 2)
(setq-default c-basic-offset 2)
;; Ctrl+h -> backspace
(global-set-key "\C-h" 'backward-delete-char)
;; assign null-function for beep
(setq ring-bell-function 'ignore)
;; chmod +x if file begins with "#!"
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; auto-revert-mode
(setq global-auto-revert-mode t)
;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode
                                      'js2-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :strike-through nil
                                                 :underline t))))))))

;; ミニバッファ履歴リストの最大長：tなら無限
(setq history-length t)
;; reverse other-window
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

;; 行末の空白を自動削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that, for
;; example.
(setq-default indent-tabs-mode nil)
;; if indent-tabs-mode is off, untabify before saving
(defun text-change-tabs-to-spaces-before-save ()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max))))
(add-hook 'before-save-hook
          'text-change-tabs-to-spaces-before-save)
(text-change-tabs-to-spaces-before-save)
;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-dired-mode)

;; auto-save-buffers-enhanced
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 30)
(auto-save-buffers-enhanced t)

;; Open symlink, not real file
(setq vc-follow-symlinks nil)

;; Region
(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)
(transient-mark-mode t)

(require 'hl-todo)
(setq hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("FIXME" . "#cc9393")
    ("XXX"   . "#cc9393")
    ("XXXX"  . "#cc9393")
    ("???"   . "#cc9393")))
(setq hl-todo-activate-in-modes
      '(prog-mode ruby-mode enh-ruby-mode gfm-mode markdown-mode))
(global-hl-todo-mode 1)

;; terminal colors
(when window-system
  (setq
   term-default-fg-color "White"
   term-default-bg-color "Black"
   ansi-term-color-vector
        [unspecified "black" "#ff5555" "#55ff55" "#ffff55" "#5555ff"
         "#ff55ff" "#55ffff" "white"]))

;; Delete region C-d
(delete-selection-mode t)

;; truncate lines setting
(require 'adaptive-wrap)
(with-eval-after-load 'adaptive-wrap
  (setq-default adaptive-wrap-extra-indent 2))
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; Disable close-other-windows
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;; Avoid to write `package-selected-packages` in init.el
(load (setq custom-file (expand-file-name "elisp/custom.el" user-emacs-directory)))

(defun diff-mode-setup-faces ()
  (set-face-attribute 'diff-added-face nil
                      :foreground "green"
                      :background nil
                      :weight 'normal)
  (set-face-attribute 'diff-removed-face nil
                      :foreground "firebrick1"
                      :background nil
                      :weight 'normal)
  (set-face-attribute 'diff-refine-change nil
                      :background "gray12"
                      :weight 'extra-bold)
  (set-face-attribute 'diff-header-face nil
                      :background "midnight blue")
  (set-face-attribute 'diff-file-header-face nil
                      :foreground "DarkOrange1"
                      :background "midnight blue"
                      :weight 'extra-bold)
  (set-face-attribute 'diff-hunk-header-face nil
                      :foreground "deep sky blue"
                      :background "midnight blue"
                      :weight 'extra-bold)
  )
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

(provide 'init_setting)
;; init_setting.el ends here

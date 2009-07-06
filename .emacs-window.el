;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
(tool-bar-mode nil)

(setq fontname (cond
                ((string-match "rust-worker" system-name)
                 "M+2VM+IPAG circle-8")
                ((string-match "precision" system-name)
                 "M+2VM+IPAG circle-9")))
(add-to-list 'default-frame-alist (cons 'font fontname))
(set-default-font fontname)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  (cons fontname "unicode-bmp"))

(require 'color-theme)
;; (load "my-color-theme-window")
;; (my-color-theme-window)
(color-theme-initialize)
(color-theme-hober)

;; transparent
(add-to-list 'default-frame-alist '(alpha . 85))
(set-frame-parameter nil 'alpha 85)
(setq frame-alpha-lower-limit 85)

;; high-light current line
(defface hlline-face
  '((((class color)
      (background dark))
     ;;(:background "dark state gray"))
     (:background "gray10"
                  :underline "gray24"))
    (((class color)
      (background light))
     (:background "ForestGreen"
                  :underline nil))
    (t ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;;(setq hl-line-face 'underline)
(global-hl-line-mode)

;; iiimcf
(setq load-path (cons (expand-file-name "~/.emacs.d/iiimcf/") load-path))
(setq iiimcf-server-control-hostlist (list (concat "/tmp/.iiim-" (user-login-name) "/:0.0")))
(when (and (= 0 (shell-command
                 (concat
                  "netstat --unix -l | grep -q " (car iiimcf-server-control-hostlist))))
           (require 'iiimcf-sc nil t))
  (setq iiimcf-server-control-default-language "ja")
  (setq iiimcf-server-control-default-input-method "atokx3")
  (setq default-input-method 'iiim-server-control)
  (setq iiimcf-UI-input-method-title-format "<ATOK:%s>")
  (progn
      ;;; キーバインドの追加
    ;; from http://okutomi-lab.ctrl.titech.ac.jp/~tkanda/atok_x3_install.htm
    ;; http://genmei.itline.jp/~svx/diary/?date=20071220
    ;; C-k/C-l        文節区切り収縮/伸張
    ;; C-i/C-o        同上
    ;; C-g    変換キャンセル
    ;; C-p    前候補
    ;; C-n    次候補グループ
    ;; C-b/C-f        文節前移動/文節後移動
    ;; C-b/C-f は文節移動にしてありますが，
    ;; ただし、ATOK 側で F2/F3 を文節移動に割り振っておく。
    (setq iiimcf-keycode-spec-alist
          (append
           '((11 37 65535) ; C-k
             (12 39 65535) ; C-l
             (9 37 65535)  ; C-i
             (15 39 65535) ; C-o
             (7 27 65535)  ; C-g
             (16 38 65535) ; C-p
             (14 28 65535) ; C-n
             (2 113 65535) ; C-b
             (6 114 65535) ; C-f
             )
           iiimcf-keycode-spec-alist))))
;; key-bind
(global-set-key [henkan] 'toggle-input-method)
(global-set-key "\C-o" 'toggle-input-method)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dired-header ((t (:foreground "aquamarine"))))
 '(font-lock-constant-face ((t (:foreground "purple"))))
 '(font-lock-function-name-face ((t (:foreground "#4186be" :weight extra-bold))))
 '(font-lock-keyword-face ((t (:foreground "#00ffff" :weight extra-bold))))
 '(font-lock-type-face ((t (:foreground "green" :weight extra-bold))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :weight bold))))
;; '(iiimcf-UI-preedit-highlight-face ((t (:foreground "DarkSlateGray1" :background "black"))))
 '(iiimcf-UI-preedit-reverse-face ((t (:foreground "DarkSlateGray1" :background "black"))))
;; '(iiimcf-UI-preedit-underline-face ((t (:foreground "DarkSlateGray1" :background "black"))))
;; '(iiimcf-UI-preedit-warning-face ((t (:foreground "DarkSlateGray1" :background "black"))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black")))))

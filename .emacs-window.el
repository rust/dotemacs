;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(howm-list-normalizer (quote howm-view-sort-by-reverse-date) t)
 '(howm-list-prefer-word nil)
 '(show-paren-mode t))

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

;; iiimcf
(setq iiimcf-server-control-hostlist (list (concat "/tmp/.iiim-" (user-login-name) "/:0.0")))
(when (and (= 0 (shell-command
                 (concat
                  "netstat --unix -l | grep -q " (car iiimcf-server-control-hostlist))))
           (require 'iiimcf-sc nil t))
  (setq iiimcf-server-control-default-language "ja")
  (setq iiimcf-server-control-default-input-method "atokx3")
  (setq default-input-method 'iiim-server-control))
(global-set-key [henkan] 'toggle-input-method)
(global-set-key "\C-o" 'toggle-input-method)
;; (require 'iiimcf)
;; (if (or (equal emacs-ime "iiimecf")
;;         (equal emacs-ime "IIIMECF")
;;         (equal emacs-ime "atokx")
;;         (equal emacs-ime "atokx2")
;;         (equal emacs-ime "atokx3"))
;;     (progn
;;       ;;; キーバインドの追加
;;       ;; from http://okutomi-lab.ctrl.titech.ac.jp/~tkanda/atok_x3_install.htm
;;       ;; http://genmei.itline.jp/~svx/diary/?date=20071220
;;       ;; C-k/C-l        文節区切り収縮/伸張
;;       ;; C-i/C-o        同上
;;       ;; C-g    変換キャンセル
;;       ;; C-p    前候補
;;       ;; C-n    次候補グループ
;;       ;; C-b/C-f        文節前移動/文節後移動
;;       ;; C-b/C-f は文節移動にしてありますが，
;;       ;; ただし、ATOK 側で F2/F3 を文節移動に割り振っておく。
;;       (setq iiimcf-keycode-spec-alist
;;             (append
;;              '((11 37 65535) ; C-k
;;                (12 39 65535) ; C-l
;;                (9 37 65535)  ; C-i
;;                (15 39 65535) ; C-o
;;                (7 27 65535)  ; C-g
;;                (16 38 65535) ; C-p
;;                (14 28 65535) ; C-n
;;                (2 113 65535) ; C-b
;;                (6 114 65535) ; C-f
;;                )
;;              iiimcf-keycode-spec-alist))))

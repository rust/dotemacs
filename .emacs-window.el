;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
;; font
;; (cond (window-system
;;        (set-default-font "font")
;;        (set-fontset-font (frame-parameter nil 'font)
;;                          'japanese-jisx0208
;;                          '("VL Gothic" . "unicode-bmp"))
;;        ))
;; (if window-system
;;     (progn
;;       ;;GUIでsystem-typeがGNU/Linuxの場合はVLゴシックを指定
;;       (cond
;;        ((eq system-type 'gnu/linux)
;;         (set-default-font "Bitstream Vera Sans Mono-10")
;;         (set-fontset-font (frame-parameter nil 'font)
;;                           'japanese-jisx0208
;;                           '("VL ゴシック" . "unicode-bmp"))
;;         ))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(howm-list-normalizer (quote howm-view-sort-by-reverse-date) t)
 '(howm-list-prefer-word nil)
 '(show-paren-mode t))

(add-to-list 'default-frame-alist '(font . "M+2VM+IPAG circle-9"))
(set-default-font "M+2VM+IPAG circle-9")
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("M+2VM+IPAG circle-9" . "unicode-bmp")))

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:stipple nil :background "black" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "bitstream" :family "M+2VM+IPAG circle")))))

(require 'color-theme)
(load "my-color-theme-window")
(my-color-theme-window)

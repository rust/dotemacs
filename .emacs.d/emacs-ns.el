;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs-ns.el
;;Color
(set-frame-parameter nil 'alpha 85)
(setq initial-frame-alist '((width . 200)(height . 120)(top . 0)(left . 45)))
;; hide menu
(display-time-mode t)
(tool-bar-mode nil)
(transient-mark-mode t)

;; frame title
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; font
(setq my-font "-apple-M+2VM+IPAG_circle-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(setq fixed-width-use-QuickDraw-for-ascii t)
(setq mac-allow-anti-aliasing t)
(if (= emacs-major-version 22)
    (require 'carbon-font))
(when (= emacs-major-version 23)
  (set-default-font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("M+2VM+IPAG circle" . "iso10646-1"))
  (setq face-font-rescale-alist
        '(("^-apple-M+2VM+IPAG_circle.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))

(require 'color-theme)
;; (load "my-color-theme-window")
;; (my-color-theme-window)
(color-theme-initialize)
(color-theme-hober)

;; high-light current line
(defface hlline-face
  '((((class color)
      (background dark))
     ;;(:background "dark state gray"))
     (:background "gray10"
                  :underline nil))
    (((class color)
      (background light))
     (:background "ForestGreen"
                  :underline nil))
    (t ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;;(setq hl-line-face 'underline)
(global-hl-line-mode)

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
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(howm-reminder-today-face ((t (:foreground "orange" :background "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black")))))

;; ;; Wanderlust
;; (require 'mime-setup)
;; (require 'wl)
;; (require 'wl-draft)
;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)

;; ;; w3m
;; (require 'w3m)
;; (require 'mime-w3m)
;; Cookie
(setq w3m-use-cookies t)
;; favicon cache
(setq w3m-favicon-cache-expire-wait nil)

;; elscreen
(add-to-list 'load-path "~/.emacs.d/apel")
(add-to-list 'load-path "~/.emacs.d/elscreen")
(require 'elscreen)
(require 'elscreen-gf)
(require 'elscreen-howm)
(require 'elscreen-w3m)
(global-set-key (kbd "C-z SPC") 'elscreen-next)
(global-set-key (kbd "C-z DEL") 'elscreen-previous)

;; window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; emoji
(add-to-list 'load-path "~/.emacs.d/emoji")
(require 'emoji)

(tool-bar-mode nil)
(global-linum-mode)

(define-key global-map [165] nil)
(define-key global-map [67109029] nil)
(define-key global-map [134217893] nil)
(define-key global-map [201326757] nil)
(define-key function-key-map [165] [?\\])
(define-key function-key-map [67109029] [?\C-\\])
(define-key function-key-map [134217893] [?\M-\\])
(define-key function-key-map [201326757] [?\C-\M-\\])

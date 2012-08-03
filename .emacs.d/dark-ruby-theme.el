(deftheme dark-ruby
  "Created 2012-08-03.")

(custom-theme-set-variables
 'dark-ruby
 '(weblogger-config-alist (quote (("stnard.jp" ("user" . "rust") ("server-url" . "http://stnard.jp/wp/xmlrpc.php") ("weblog" . "1")))))
 '(show-paren-mode t)
 '(column-number-mode t))

(custom-theme-set-faces
 'dark-ruby
 '(dired-header ((t (:foreground "aquamarine"))))
 '(font-lock-constant-face ((t (:foreground "purple"))))
 '(font-lock-function-name-face ((t (:foreground "#4186be" :weight extra-bold))))
 '(font-lock-keyword-face ((t (:foreground "#00ffff" :weight extra-bold))))
 '(font-lock-type-face ((t (:foreground "green" :weight extra-bold))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :weight bold))))
 '(erb-comment-delim-face ((t (:inherit erb-delim-face :foreground "red" :weight bold))))
 '(erb-comment-face ((t (:inherit erb-face :foreground "red" :weight bold))))
 '(erb-delim-face ((t (:background "#383838"))))
 '(erb-face ((t (:background "#383838"))))
 '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "pink4" :weight bold))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "gray29"))))
 '(howm-reminder-today-face ((t (:foreground "orange" :background "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black"))))
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(provide-theme 'dark-ruby)

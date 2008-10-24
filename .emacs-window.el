;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/iiimcf/") load-path))

;; font
(set-default-font "Mono-8")
(set-fontset-font (frame-parameter nil 'font)
		  'japanese-jisx0208
		  '("M+1MN+IPAG" . "unicode-bmp"))

;; input-method
(setq iiimcf-server-control-hostlist '("/tmp/.iiim-rust/:0.0"))
(setq iiimcf-server-control-username "rust@worker")
(setq iiimcf-server-control-default-language "ja")
(setq iiimcf-server-control-default-input-method "atokx3")
(require 'iiimcf-sc)
(setq default-input-method 'iiim-server-control)
(define-key global-map [henkan] 'toggle-input-method)
(define-key global-map [zenkaku-hankaku] 'toggle-input-method)
(define-key global-map [M-zenkaku-hankaku] 'toggle-input-method)
;;(define-key global-map "\C-\ " 'set-mark-command)

;; Startup message
(setq inhibit-startup-message t)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; turn on font-lock mode
;; (when (fboundp 'global-font-lock-mode)
;;   (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(temp-buffer-resize-mode 1)
(line-number-mode t)
(column-number-mode t)

;; wb-line-number
(require 'wb-line-number)
(wb-line-number-toggle)
(setq truncate-partial-width-windows nil)
(set-scroll-bar-mode nil)
(setq wb-line-number-scroll-bar t)

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)
(global-font-lock-mode t)

(show-paren-mode t) ; 対応する括弧を光らせる。
(transient-mark-mode t) ; 選択部分のハイライト

;; install-elips
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

;;(load "emacs21-256color.el")

;; rails
(defun try-complete-abbrev (old)
       (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

(setq load-path (cons "~/.emacs.d/emacs-rails" load-path))
(require 'rails)
(setq rails-ri-command "fri")

;; mmm-mode
(add-to-list 'load-path "~/.emacs.d/mmm-mode")
(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
;;(set-face-background 'mmm-output-submode-face "LightBlue")
;;(set-face-background 'mmm-code-submode-face "LightGray")
;;(set-face-background 'mmm-comment-submode-face "LightYellow")
;;(set-face-background 'mmm-special-submode-face "Yellow")

(mmm-add-classes
  '((erb-code
     :submode ruby-mode
     :match-face (("<%#" . mmm-comment-submode-face)
                  ("<%=" . mmm-output-submode-face)
                  ("<%"  . mmm-code-submode-face))
     :front "<%[#=]?"
     :back "%>"
     :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
              (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
              (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
     )))
(mmm-add-classes
  '((gettext
     :submode gettext-mode
     :front "_(['\"]"
     :face mmm-special-submode-face
     :back "[\"'])")))
(mmm-add-classes
  '((html-script
     :submode javascript-mode
     :front "<script>"
     :back "</script>")))

(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

(add-hook 'html-mode-hook
          (lambda()
             (setq mmm-classes '(erb-code html-js html-script gettext emmbeded-css))
             (mmm-mode-on)))

(add-to-list 'mmm-mode-ext-classes-alist '(ruby-mode nil gettext))

(global-set-key [f8] 'mmm-parse-buffer)

;; ruby
(require 'ruby-mode)
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-keys)
;; auto-mode by ruby
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-mode))
;; indent
(setq ruby-deep-indent-paren-style nil)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda()(ruby-electric-mode 1)))
(setq ruby-electric-expand-delimiters-list '( ?\{))

;; fastri
(setq ri-ruby-script "/usr/local/bin/ri-emacs")
(load "ri-ruby")

(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun make-ruby-scratch-buffer ()
  (with-current-buffer (get-buffer-create "*ruby scratch*")
		       (ruby-mode)
		       (current-buffer)))
(defun ruby-scratch ()
  (interactive)
  (pop-to-buffer (make-ruby-scratch-buffer)))
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\C-f\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

;; ecb
(load-file "~/.emacs.d/cedet-1.0pre4/common/cedet.el")
(semantic-load-enable-code-helpers)
(add-to-list 'load-path "~/.emacs.d/ecb-2.32")
(require 'ecb)
;;(require 'ecb-autoloads)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)

(defun ecb-toggle ()
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate) 
    (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)

;; svn
(autoload 'svn-status "psvn" nil t)
(add-hook 'dired-mode-hook
          '(lambda ()
             (require 'dired-x)
             ;;(define-key dired-mode-map "V" 'cvs-examine)
             (define-key dired-mode-map "V" 'svn-status)
             (turn-on-font-lock)
             ))
(setq svn-status-hide-unmodified t)
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; ecb
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-minor-mode-text "")
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("~/public_html/"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erb-delim-face ((t (:background "color-78" :foreground "brightblack"))))
 '(erb-exec-face ((t (:inherit erb-face :background "black"))))
 '(erb-face ((t (:background "color-233"))))
 '(flymake-errline ((((class color)) (:background "color-160"))))
 '(mmm-cleanup-submode-face ((t (:foreground "color-184"))))
 '(mmm-code-submode-face ((t (:foreground "color-34"))))
 '(mmm-comment-submode-face ((t (:foreground "brightcyan"))))
 '(mmm-declaration-submode-face ((t (:foreground "color-159"))))
 '(mmm-default-submode-face ((t (:foreground "color-251" :weight normal))))
 '(mmm-init-submode-face ((t (:foreground "color-213"))))
 '(mmm-output-submode-face ((t (:foreground "color-162"))))
 '(mmm-special-submode-face ((t (:foreground "color-118"))))
 '(tool-bar ((default (:foreground "white" :box (:line-width 1 :style released-button))) (nil nil))))

;;; yaml-mode の設定
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq comment-start "#")
             (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
             (setq comment-end-skip "$")
             (set (make-local-variable 'comment-style) 'indent) ))

;; howm
(autoload 'howm-menu "howm-mode" "Howm mode" t)
(autoload 'howm-list-all "howm-mode" "Howm mode" t)
(autoload 'howm-create "howm-mode" "Howm mode" t)
(global-set-key "\C-c,," 'howm-menu)
(global-set-key "\C-c,a" 'howm-list-all)
(global-set-key "\C-c,c" 'howm-create)
(setq howm-directory "~/howm")          ;; memoの場所
(setq howm-view-summary-persistent nil) ;; ファイルを開く際に一覧を消す
(setq howm-list-recent-title t)         ;; 最近のメモ時にタイトル表示
(setq howm-list-all-title t)            ;; 一覧時にタイトル表示
(setq howm-menu-expiry-hours 2)         ;; ２時間キャッシュ
(add-hook 'howm-mode-on-hook 'auto-fill-mode)
(setq howm-view-summary-persistent nil)
;;
(setq howm-menu-schedule-days-before 10) ;; 10日前から
(setq howm-menu-schedule-days 3)         ;; 3日後まで
;;
(setq howm-refresh-after-save nil)
(setq howm-menu-refresh-after-save nil)
(setq howm-menu-recent-num 10)
(setq howm-menu-todo-num 10)
(setq howm-view-keep-one-window t)
(setq howm-list-normalizer 'howm-view-sort-by-mtime)

;; for M-x align
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

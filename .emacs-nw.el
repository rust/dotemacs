;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/iiimcf/") load-path))

;; Startup message
(setq inhibit-startup-message t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
;; メニューバーを消す
(menu-bar-mode nil)

;; default to unified diffs
(setq diff-switches "-u")

(temp-buffer-resize-mode 1)
(line-number-mode t)
(column-number-mode t)

;; wb-line-number
(require 'wb-line-number)
(wb-line-number-toggle)
;;(setq truncate-partial-width-windows nil)
(set-scroll-bar-mode nil)
(setq wb-line-number-scroll-bar t)

;; 256 colors
(load "emacs21-256color-hack.el")
(require 'color-theme)
(load "my-color-theme")
(my-color-theme)

;; ;; flymake color settings
;; (custom-set-faces
;;   '(flymake-errline ((((class color)) (:background "Gray30"))))
;;   '(flymake-warnline ((((class color)) (:background "Gray55")))))

;; 折り返し
;;(setq truncate-line nil)

(show-paren-mode t) ; 対応する括弧を光らせる。
(transient-mark-mode t) ; 選択部分のハイライト

;; install-elips
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

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
;;(add-hook 'howm-mode-on-hook 'auto-fill-mode)
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

;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;; Gauche
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferir Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode))))
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
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))))))

;; ミニバッファ履歴リストの最大長：tなら無限
(setq history-length t)
;; session.el
;;   kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1))
;; minibuf-isearch
;;   minibufでisearchを使えるようにする
(require 'minibuf-isearch nil t)

;; css-mode
(require 'css-mode)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; reverse other-window
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

;; Outputz
(require 'outputz)
(setq outputz-key "Q.Oe0fNtr-XZ")      ;; 復活の呪文
(setq outputz-uri "http://stnard.jp/%s") ;; 適当なURL。%sにmajor-modeの名前が入るので、major-modeごとのURLで投稿できます。
(global-outputz-mode t)

;; snippet for RSpec
(require 'snippet)
(add-hook 'rails-minor-mode-hook
          '(lambda()
             (setq-default abbrev-mode t)
             (snippet-with-abbrev-table 'local-abbrev-table
                                        ("it" ."it \"$${spec}\" do\n$>$.\nend$>\n")
                                        ("sbt"."should be_true")
                                        ("sbi"."should be_an_instance_of($${klass})")
                                        ("se"."shoud == ")
                                        )
             ))

;; 行末の空白を自動削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; タブを空白に自動変換
(defun untabify-before-save ()
  (untabify 1 (point-max)))
(add-hook 'before-save-hook 'untabify-before-save)

;;; Interactively Do Things
(require 'ido)
(ido-mode t)

;; ;; mmm-mode
;; (require 'mmm-mode)
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 2)

;; ;; nxml-mode & rhtml
;; (add-to-list 'load-path "~/.emacs.d/rhtml")
;; (require 'nxml-mode)
;; (require 'rhtml-mode)
;; (require 'rhtml-erb)

;; ;; rails
;; (defun try-complete-abbrev (old)
;;        (if (expand-abbrev) t nil))
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))

;; (setq load-path (cons "~/.emacs.d/emacs-rails" load-path))
;; (require 'rails)
;; (setq rails-ri-command "fri")

;; (add-hook 'nxml-mode-hook
;;           (lambda ()
;;             (define-key nxml-mode-map "r" 'newline-and-indent)
;;             (setq indent-tabs-mode nil)
;;             (setq local-abbrev-table nxml-mode-abbrev-table)
;;             (message "My nxml-mode customizations loaded")))

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
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

;;; yaml-mode の設定
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq comment-start "#")
             (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
             (setq comment-end-skip "$")
             (set (make-local-variable 'comment-style) 'indent) ))

;; for M-x align
(require 'align)
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

;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml-mode")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

;; yasnippet
(setq load-path (cons (expand-file-name "~/.emacs.d/yasnippet-0.5.7") load-path))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippets-rails/rails-snippets/")


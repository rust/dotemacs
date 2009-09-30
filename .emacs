;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs
(put 'upcase-region 'disabled nil)

;; load-path
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

;; Startup message
(setq inhibit-startup-message t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

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
;; \t を使わない
(setq-default indent-tabs-mode nil)

;; install-elips
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

;; howm
(setq load-path (cons (expand-file-name "~/.emacs.d/howm") load-path))
(require 'howm)
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
(setq howm-normalizer 'howm-view-sort-by-reverse-date)
(setq howm-list-prefer-word nil)
(add-to-list 'auto-mode-alist '("\\.howm$" . howm-mode))

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
(require 'typing-outputz)
(global-typing-outputz-mode t)
(load "~/.outputz.el")

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

;; ruby
(require 'ruby-mode)
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-keys)
;; auto-mode by ruby
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
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

;; for rabbit-mode
(autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
(add-to-list 'auto-mode-alist '("\\.\\(rbt\\|rab\\)$" . rabbit-mode))

;; for auto-complete
(require 'auto-complete-ruby)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))))

;; YaTeX が漢字コードを毎回 ISO-2022-JP に設定しないようにする
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-AMS-LaTeX t)

;; git.el をロードする
(load-library "~/.emacs.d/git.el")

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset       2
                   tab-width              2
                   indent-tabs-mode       nil
                   js2-bounce-indent-flag nil
                   js-indent-level        2
                   js-mirror-mode         nil
                   js2-cleanup-whitespace nil)))

;; for haskell
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; anything
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "C-x b") 'anything)

;; descbinds-anything
(require 'descbinds-anything)
(descbinds-anything-install)

;; ac-complete.el
(require 'ac-anything)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)

;; truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; emacs-nav
(add-to-list 'load-path "~/.emacs.d/emacs-nav/")
(require 'nav)
(setq nav-width 12)

;;; sdic-mode 用の設定
(add-to-list 'load-path "~/.emacs.d/sdic")
(require 'sdic)
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; ;; multi-term
;; (require 'multi-term)
;; (setq multi-term-program "/usr/bin/zsh")
;; (global-set-key "\C-t" 'multi-term)
;; (setq multi-term-dedicated-window-height 10)
;; (setq multi-term-dedicated-max-window-height 20)
;; (setq term-unbind-key-list (quote ("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")))

;; shell-pop
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/usr/bin/zsh")
(shell-pop-set-window-height 20)
(defvar ansi-term-after-hook nil)
(add-hook 'ansi-term-after-hook
          (function
           (lambda ()
             (define-key term-raw-map "\C-t" 'shell-pop))))
(defadvice ansi-term (after ansi-term-after-advice (arg))
  "run hook as after advice"
  (run-hooks 'ansi-term-after-hook))
(ad-activate 'ansi-term)
(global-set-key "\C-t" 'shell-pop)
(defun shell-pop-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

;; terminal colors
(when window-system
  (setq
   term-default-fg-color "White"
   term-default-bg-color "Black"
   ansi-term-color-vector
        [unspecified "black" "#ff5555" "#55ff55" "#ffff55" "#5555ff"
         "#ff55ff" "#55ffff" "white"]))

;; org-mode + remember-mode
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats (quote ("<%Y年%m月%d日(%a)>" . "<%Y年%m月%d日(%a)%H時%M分>")))
(define-key global-map "\C-cr" 'org-remember)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t))

;; Ctrl+h -> backspace
(global-set-key "\C-h" 'backward-delete-char)

;; smalltalk
(require 'smalltalk-mode)
(require 'gst-mode)

;; hatena-mode
(setq load-path (cons (expand-file-name "~/.emacs.d/hatena-mode") load-path))
(load "hatena-mode")
(setq hatena-usrid "conceal-rs")
(setq hatena-plugin-directory "~/.emacs.d/hatena-mode")

;; Wanderlust
(require 'mime-setup)
(require 'wl)
(require 'wl-draft)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)

;; window or no-window
(cond
 ((eq window-system 'x)
  (setq default-frame-alist
        (append (list '(foreground-color . "black"))))
  (load "~/.emacs-window.el"))
 ((null window-system)
  (load "~/.emacs-nw.el")))

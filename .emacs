;; .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; default settings
(put 'upcase-region 'disabled nil) ;; 大文字変換を無効化
;; load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/ruby/")
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(add-to-list 'load-path "~/.emacs.d/remember/")
(add-to-list 'load-path "~/.emacs.d/org-mode/")
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "~/.emacs.d/howm/")
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.5.7/")
(add-to-list 'load-path "~/.emacs.d/rinari/")
(add-to-list 'load-path "~/.emacs.d/rhtml-mode/")
(add-to-list 'load-path "~/.emacs.d/yatex/")
(add-to-list 'load-path "~/.emacs.d/hatena-mode/")
(add-to-list 'load-path "~/.emacs.d/emacs-nav/")
(add-to-list 'load-path "~/.emacs.d/sdic/")
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(add-to-list 'load-path "/usr/local/scala/misc/scala-tool-support/emacs")
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
;; \t を使わない
(setq-default indent-tabs-mode nil)
;; truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
;; Ctrl+h -> backspace
(global-set-key "\C-h" 'backward-delete-char)
;; assign null-function for beep
(setq ring-bell-function 'ignore)
;; chmod +x if file begins with "#!"
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install-elips
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")
;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; howm
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
(setq howm-list-normalizer 'howm-view-sort-by-reverse-date)
;; (setq howm-normalizer 'howm-view-sort-by-reverse-date)
(setq howm-list-prefer-word nil)
(add-to-list 'auto-mode-alist '("\\.howm$" . howm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ミニバッファ履歴リストの最大長：tなら無限
(setq history-length t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css-mode
(require 'css-mode)
(setq cssm-indent-function #'cssm-c-style-indenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reverse other-window
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputz
(require 'typing-outputz)
(global-typing-outputz-mode t)
(load "~/.outputz.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行末の空白を自動削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; タブを空白に自動変換
(defun untabify-before-save ()
  (untabify 1 (point-max)))
(add-hook 'before-save-hook 'untabify-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively Do Things
(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; rcodetools
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
;; for rabbit-mode
(autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
(add-to-list 'auto-mode-alist '("\\.\\(rbt\\|rab\\)$" . rabbit-mode))
;; ;; for auto-complete
;; (require 'auto-complete-ruby)
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))))
;; るりま
(require 'anything-rurima)
(setq anything-rurima-index-file "~/Dropbox/rurima/rubydoc/rurima.e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yaml-mode の設定
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq comment-start "#")
             (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
             (setq comment-end-skip "$")
             (set (make-local-variable 'comment-style) 'indent) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for rails
;;;; Rinari
(require 'rinari)
;;;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
;;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippets-rails/rails-snippets/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for tex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-AMS-LaTeX t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; fixing indentation
;; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "C-x b") 'anything)
;; descbinds-anything
(require 'descbinds-anything)
(descbinds-anything-install)
;; ;; ac-complete.el
;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-nav
(require 'nav)
(setq nav-width 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sdic-mode 用の設定
(require 'sdic)
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-pop
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(cond
 ((eq window-system 'ns) (shell-pop-set-internal-mode-shell "/opt/local/bin/zsh"))
 ((eq window-system 'x) (shell-pop-set-internal-mode-shell "/usr/bin/zsh")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminal colors
(when window-system
  (setq
   term-default-fg-color "White"
   term-default-bg-color "Black"
   ansi-term-color-vector
        [unspecified "black" "#ff5555" "#55ff55" "#ffff55" "#5555ff"
         "#ff55ff" "#55ffff" "white"]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; org-mode + remember-mode
;; (require 'org-install)
;; (setq org-startup-truncated nil)
;; (setq org-return-follows-link t)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (org-remember-insinuate)
;; (setq org-directory "~/memo/")
;; (setq org-default-notes-file (concat org-directory "agenda.org"))
;; (setq org-remember-templates
;;       '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
;;         ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
;;         ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
;;         ))
;; (setq org-display-custom-times t)
;; (setq org-time-stamp-custom-formats (quote ("<%Y年%m月%d日(%a)>" . "<%Y年%m月%d日(%a)%H時%M分>")))
;; (define-key global-map "\C-cr" 'org-remember)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smalltalk
(require 'smalltalk-mode)
(require 'gst-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hatena-mode
(load "hatena-mode")
(setq hatena-usrid "conceal-rs")
(setq hatena-plugin-directory "~/.emacs.d/hatena-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala
(require 'scala-mode-auto)
(setq scala-interpreter "/usr/local/bin/scala")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hiki-mode
(load "~/.hiki.el")
;; 更新の際に browser を起動したいなら有効に
;;(setq hiki-browser-function 'browse-url)
(autoload 'hiki-edit "hiki-mode" nil t)
(autoload 'hiki-edit-url "hiki-mode" nil t)
(autoload 'hiki-index "hiki-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
;; for global
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)
(setq ac-dwim t)
;; sources
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hool (lambda () (add-to-list 'ac-sources 'ac-source-symbold t)))
;; automatic completion
(setq ac-auto-start 5)
(ac-set-trigger-key "TAB")
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TDD mode-line
(defvar tdd-color-alist
  '(("Think"       "white" "darkslateblue")
    ("Red"         "white" "#ff4444")
    ("Green"       "white" "#44dd44")
    ("Refactoring" "white" "#ffaa44")))

(defvar tdd-bgcolor-mode 3)
(defvar tdd-bgcolor-mode-name "")
(let ((cell (or (memq 'mode-line-position mode-line-format)
                (memq 'mode-line-buffer-identification mode-line-format)))
      (newcdr 'tdd-bgcolor-mode-name))
  (unless (member newcdr mode-line-format)
    (setcdr cell (cons newcdr (cdr cell)))))

(defun tdd-bgcolor-rotate ()
  (interactive)
  (let (pair)
    (if (>= tdd-bgcolor-mode 3)
        (setq tdd-bgcolor-mode 0)
      (setq tdd-bgcolor-mode
            (+ tdd-bgcolor-mode 1)))
    (setq pair
          (nth tdd-bgcolor-mode tdd-color-alist))
    (setq tdd-bgcolor-mode-name (format "[%s]" (car pair)))
    (message tdd-bgcolor-mode-name)
    (set-face-foreground 'mode-line (cadr pair))
    (set-face-background 'mode-line (caddr pair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window or no-window
(cond
 ((eq window-system 'x)
  (setq default-frame-alist
        (append (list '(foreground-color . "black"))))
  (load "emacs-window"))
 ((eq window-system 'ns)
  (load "emacs-ns"))
 ((eq window-system 'mac)
  (load "emacs-ns"))
 ((null window-system)
  (load "emacs-nw")))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t))

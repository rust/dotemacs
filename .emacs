;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuf-isearch
;;   minibufでisearchを使えるようにする
(require 'minibuf-isearch nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css-mode
(require 'css-mode)
(setq cssm-indent-function #'cssm-c-style-indenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively Do Things
(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets/")
;;(require 'dropdown-list)
;;(setq yas/prompt-functions '(yas/dropdown-prompt))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-nav
(require 'nav)
(setq nav-width 12)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; sdic-mode 用の設定
;; (require 'sdic)
;; (autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
;; (global-set-key "\C-cw" 'sdic-describe-word)
;; (autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
;; (global-set-key "\C-cW" 'sdic-describe-word-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-pop
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(cond
 ((eq window-system 'ns) (shell-pop-set-internal-mode-shell "/opt/local/bin/zsh"))
 ((eq window-system 'mac) (shell-pop-set-internal-mode-shell "/opt/local/bin/zsh"))
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; hatena-mode
;; (load "hatena-mode")
;; (setq hatena-usrid "conceal-rs")
;; (setq hatena-plugin-directory "~/.emacs.d/elisp/hatena-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala
(require 'scala-mode-auto)
(cond
 ((eq window-system 'ns) (setq scala-interpreter "/opt/local/bin/scala"))
 ((eq window-system 'mac) (setq scala-interpreter "/opt/local/bin/scala"))
 ((eq window-system 'x) (setq scala-interpreter "/usr/local/scala/bin/scala")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hiki-mode
(load "~/.hiki.el")
;; 更新の際に browser を起動したいなら有効に
;;(setq hiki-browser-function 'browse-url)
(autoload 'hiki-edit "hiki-mode" nil t)
(autoload 'hiki-edit-url "hiki-mode" nil t)
(autoload 'hiki-index "hiki-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pop-tip.el
(require 'pos-tip)
;; auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/dict")
(require 'auto-complete-config)
(ac-config-default)
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
(setq ac-quick-help-prefer-x t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSense
;;(setq rsense-home "/opt/rsense/")
;;(add-to-list 'load-path (concat rsense-home "/etc"))
;;(require 'rsense)
;;(add-hook 'ruby-mode-hook
;;          (lambda ()
;;            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))
;;(add-hook 'ruby-mode-hook
;;          (lambda ()
;;            (add-to-list 'ac-sources 'ac-source-rsense-method)
;;            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; undo-tree.el
;; (require 'undo-tree)
;; (global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphviz-mode.el
(load "~/.emacs.d/elisp/graphviz-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit.el
(add-to-list 'load-path "~/.emacs.d/elisp/magit/")
(require 'magit)
(require 'git-blame)
 ;; for emacs-22
(if (= emacs-major-version 22)
    (defalias 'start-file-process 'start-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D Proguraming Language
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-hook 'd-mode-hook
          '(lambda()
             '(c-toggle-auto-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WordPress mode
(load-file "~/.emacs.d/elisp/wp-emacs/weblogger.el")
(global-set-key "\C-cbs" 'weblogger-start-entry)
(load "~/.wordpress.el")
(add-hook 'weblogger-entry-mode-hook
          '(lambda()
             (setq auto-fill-mode f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; window or no-window
;; (cond
;;  ((eq window-system 'x)
;;   (setq default-frame-alist
;;         (append (list '(foreground-color . "black"))))
;;   (load "~/.emacs.d/emacs-window"))
;;  ((eq window-system 'ns)
;;   (load "~/.emacs.d/emacs-ns"))
;;  ((eq window-system 'mac)
;;   (load "~/.emacs.d/emacs-ns"))
;;  ((null window-system)
;;   (load "~/.emacs.d/emacs-nw")))


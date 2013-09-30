;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"  . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header-file
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css-mode
(require 'css-mode)
(setq cssm-indent-function #'cssm-c-style-indenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively Do Things
(require 'ido)
(ido-mode t)

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
;; for tex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-AMS-LaTeX t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminal colors
(when window-system
  (setq
   term-default-fg-color "White"
   term-default-bg-color "Black"
   ansi-term-color-vector
        [unspecified "black" "#ff5555" "#55ff55" "#ffff55" "#5555ff"
         "#ff55ff" "#55ffff" "white"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


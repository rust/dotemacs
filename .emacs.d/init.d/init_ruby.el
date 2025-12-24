;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ruby.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby

(add-to-list 'treesit-language-source-alist
             '(ruby "https://github.com/tree-sitter/tree-sitter-ruby"))
(treesit-install-language-grammar 'ruby)

;; auto-mode by ruby
(use-package ruby-ts-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\.tmp" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Schemafile" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.schema$" . ruby-ts-mode)))

(use-package yard-mode
  :ensure t
  :config
  (add-hook 'ruby-mode 'yard-mode))

;; inf-ruby
(use-package inf-ruby
  :ensure t
  :config
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

;; for M-x align
(use-package align
  :ensure t
  :config
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
                 (modes  . '(ruby-mode)))))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(use-package rubocop
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode))

(provide 'init_ruby)
;; init_ruby.el ends here

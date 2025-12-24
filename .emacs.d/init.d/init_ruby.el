;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ruby.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby

;; auto-mode by ruby
(use-package ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\.tmp" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Schemafile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.schema$" . ruby-mode))

  (defun ruby-mode-set-encoding () ())

  ;; coding style
  (electric-pair-mode t)
  (add-to-list 'electric-pair-pairs '(?| . ?|))
  )

(use-package yard-mode
  :ensure t
  :config
  (add-hook 'ruby-mode 'yard-mode))

;; highlight block
(use-package ruby-block
  :ensure t
  :config
  (setq ruby-block-highlight-toggle t))

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

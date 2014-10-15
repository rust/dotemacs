;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ruby.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(setq enh-ruby-program "~/.rbenv/shims/ruby")
(require 'enh-ruby-mode)
;; auto-mode by ruby
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\.tmp" . ruby-mode))
;; ;; indent
(require 'ruby-end)
(add-hook 'enh-ruby-mode-hook 'ruby-end-mode)
;; (require 'ruby-electric)
;; (add-hook 'enh-ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;; (setq ruby-electric-expand-delimiters-list nil)
(defun make-ruby-scratch-buffer ()
  (with-current-buffer (get-buffer-create "*ruby scratch*")
    (ruby-mode)
    (current-buffer)))
(defun ruby-scratch ()
  (interactive)
  (pop-to-buffer (make-ruby-scratch-buffer)))
;; for rabbit-mode
(autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" t)
(add-to-list 'auto-mode-alist '("\\.\\(rbt\\|rab\\)$" . rabbit-mode))

(setq ruby-deep-indent-paren-style nil)
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-check-syntax nil)

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{128\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(defun enh-ruby-mode-set-encoding () ())

(require 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;;; http://www.emacswiki.org/emacs/ruby-block.el
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

(provide 'init_ruby)
;; init_ruby.el ends here

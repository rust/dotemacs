;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_ruby.el

;; Copyright (C) 2010 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(require 'ruby-mode)
;; auto-mode by ruby
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\.tmp" . ruby-mode))
;; ;; indent
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))
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

(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{128\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(defun ruby-mode-set-encoding () ())

(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

;;; http://www.emacswiki.org/emacs/ruby-block.el
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

(provide 'init_ruby)
;; init_ruby.el ends here

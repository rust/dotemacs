;;; init_ruby.el --- Ruby設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Ruby開発環境の設定。
;; tree-sitterベースのruby-modeとその関連設定を含む。

;;; Code:

;; ruby

;; auto-mode by ruby
(use-package ruby-ts-mode
  :ensure nil
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
  :hook (ruby-ts-mode . yard-mode))

;; for M-x align
(use-package align
  :ensure nil
  :config
  (add-to-list 'align-rules-list
               '(ruby-comma-delimiter
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-ts-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-literal
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-ts-mode))))
  (add-to-list 'align-rules-list
               '(ruby-assignment-literal
                 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-ts-mode))))
  (add-to-list 'align-rules-list          ;TODO add to rcodetools.el
               '(ruby-xmpfilter-mark
                 (regexp . "\\(\\s-*\\)# => [^#\t\n]")
                 (repeat . nil)
                 (modes  . '(ruby-ts-mode)))))

(provide 'init_ruby)
;; init_ruby.el ends here

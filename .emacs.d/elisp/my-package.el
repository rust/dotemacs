;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; my-package.el

;; Copyright (C) 2015 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get packages
;;;; General
;; (el-get-bundle auto-save-buffers-enhanced)

;;;; for auto-complete
(el-get-bundle auto-complete)
(el-get-bundle fuzzy)
(el-get-bundle popup)
(el-get-bundle pos-tip)

;;;; Search
(el-get-bundle anzu)

;;;; Buffer utils
(el-get-bundle popwin)
(el-get-bundle elscreen)
(el-get-bundle yascroll)
(el-get-bundle buffer-move)

;; Region
(el-get-bundle expand-region)

;; Programming Misc
;; (el-get-bundle rainbow-mode)
(el-get-bundle vimrc-mode)

;; https://github.com/Fuco1/smartparens
(el-get-bundle Fuco1/smartparens)

;;;; flymake
(el-get-bundle flycheck)
;; (el-get-bundle flymake-jslint)

;;;; Python
(el-get-bundle jedi)
(el-get-bundle py-autopep8)
(el-get-bundle python-django)

;;;; helm
(el-get-bundle helm)
;; (el-get-bundle ac-helm)
;; (el-get-bundle helm-git)
;; (el-get-bundle helm-git-files)
(el-get-bundle helm-descbinds)
;; (el-get-bundle helm-swoop)
;; (el-get-bundle ace-isearch)

;;;; git
(el-get-bundle magit)
(el-get-bundle git-gutter)
(el-get-bundle git-gutter-fringe)
(el-get-bundle git-blame)

;;;; Ruby
(el-get-bundle rbenv)
(el-get-bundle ruby-block)
(el-get-bundle inf-ruby)
(el-get-bundle fringe-helper)
(el-get-bundle yasnippet)
(el-get-bundle rubocop)

;;;; Rails
(el-get-bundle rspec-mode)
(el-get-bundle yard-mode)
(el-get-bundle rhtml-mode)
(el-get-bundle js2-mode)
(el-get-bundle haml-mode)
(el-get-bundle slim-mode)
(el-get-bundle scss-mode)
(el-get-bundle sass-mode)
(el-get-bundle yaml-mode)

;;;; CoffeeScript
(el-get-bundle coffee-mode)
;; (el-get-bundle sourcemap)

;;;; Python
(el-get-bundle python-mode)

;;;; Misc modes
(el-get-bundle web-mode)
(el-get-bundle review-mode)
(el-get-bundle markdown-mode)
(el-get-bundle graphviz-dot-mode)
;; (el-get-bundle adaptive-wrap)

;;;; Utils
(el-get-bundle multi-term)
;; (el-get-bundle session)

;;;; Color
;; (el-get-bundle solarized-theme)
;; (el-get-bundle ample-theme)
;; (el-get-bundle hl-todo)
(el-get-bundle highlight-indentation)

;;;; Elixir
(el-get-bundle erlang-mode)
(el-get-bundle elixir-lang/emacs-elixir)
(el-get-bundle tonini/alchemist.el)
(el-get-bundle syohex/emacs-ac-alchemist)

;;;; Virtualization
(el-get-bundle dockerfile-mode)

;; infra
(el-get-bundle terraform-mode)
(el-get-bundle hcl-mode)
(el-get-bundle nginx-mode)

;; package.el and use-package
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(package-install 'use-package)

;; Utils
(package-install 'auto-save-buffers-enhanced)
(package-install 'pkg-info)
(package-install 'migemo)

;; Modes
(package-install 'rainbow-mode)
(package-install 'sourcemap)
(package-install 'adaptive-wrap)
(package-install 'toml-mode)
(package-install 'session)
(package-install 'ample-theme)
(package-install 'hl-todo)
(package-install 'jsx-mode)
(package-install 'yatex)
(package-install 'json-mode)
(package-install 'ghc)
(package-install 'go-mode)
(package-install 'go-autocomplete)
(package-install 'scala-mode)
(package-install 'ensime)
(package-install 'php-mode)
(package-install 'ac-php)
(package-install 'rust-mode)

;; Themes
(package-install 'spacemacs-theme)
(package-install 'solarized-theme)
(package-install 'powerline)

(provide 'my-package)
;; my-el-get.el ends here

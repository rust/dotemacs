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
(el-get-bundle auto-save-buffers-enhanced)
(el-get-bundle use-package)
(el-get-bundle pkg-info)
(el-get-bundle exec-path-from-shell)
(el-get-bundle rainbow-mode)

;;;; for auto-complete
(el-get-bundle fuzzy)
(el-get-bundle popup)
(el-get-bundle pos-tip)
(el-get-bundle expand-region)

;; Git
(el-get-bundle magit :info "docs")

;;;; Search
(el-get-bundle anzu)

;;;; Buffer utils
(el-get-bundle popwin)
(el-get-bundle elscreen)
(el-get-bundle yascroll)
(el-get-bundle buffer-move)

;; https://github.com/Fuco1/smartparens
(el-get-bundle Fuco1/smartparens)

;;;; flycheck
(el-get-bundle flycheck)

;;;; git
(el-get-bundle git-gutter+)
(el-get-bundle git-gutter-fringe+)

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
(el-get-bundle haml-mode)
(el-get-bundle slim-mode)
(el-get-bundle scss-mode)
(el-get-bundle sass-mode)
(el-get-bundle yaml-mode)

;;;; JavaScript
(el-get-bundle coffee-mode)
(el-get-bundle sourcemap)
(el-get-bundle js2-mode)
(el-get-bundle tide)
(el-get-bundle prettier-js)

;; Finder
(el-get-bundle vertico)

;;;; Misc modes
(el-get-bundle company-mode)
(el-get-bundle company-quickhelp)
(el-get-bundle web-mode)
(el-get-bundle review-mode)
(el-get-bundle markdown-mode)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle vue-mode)
(el-get-bundle vue-html-mode)
(el-get-bundle rainbow-delimiters)

;;;; Utils
(el-get-bundle multi-term)

;;;; Color
(el-get-bundle powerline)
(el-get-bundle solarized-theme)
(el-get-bundle highlight-indentation)

;;;; Elixir
(el-get-bundle erlang-mode)
(el-get-bundle elixir-lang/emacs-elixir)
(el-get-bundle tonini/alchemist.el)
(el-get-bundle syohex/emacs-ac-alchemist)

;; Python
(el-get-bundle python-mode)
(el-get-bundle jedi)
(el-get-bundle python-django)
(el-get-bundle py-autopep8)

;; Scala
(el-get-bundle scala-mode)

(el-get-bundle kotlin-mode)
(el-get-bundle flycheck-kotlin)

;; Misc Programming languages
(el-get-bundle php-mode)
(el-get-bundle ac-php)
(el-get-bundle rust-mode)
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle rjsx-mode)
(el-get-bundle haskell-mode)
(el-get-bundle ghc-mod)
(el-get-bundle hl-todo)
(el-get-bundle adaptive-wrap)
(el-get-bundle groovy-mode)

;; infra
(el-get-bundle terraform-mode)
(el-get-bundle hcl-mode)
(el-get-bundle nginx-mode)
(el-get-bundle dockerfile-mode)
(el-get-bundle docker-compose-mode)
(el-get-bundle toml-mode)
(el-get-bundle json-mode)
(el-get-bundle jsonnet-mode)
(el-get-bundle vimrc-mode)

(el-get 'sync)

(provide 'my-package)
;; my-el-get.el ends here

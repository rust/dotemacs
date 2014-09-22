;; Refresh packages
(package-refresh-contents)

;; Targer packages
(defvar my/favorite-packages
  '(
    ;;;; General
    color-theme-solarized auto-save-buffers-enhanced highlight-indentation

    ;;;; for auto-complete
    auto-complete fuzzy popup pos-tip anzu

    ;;;; buffer utils
    popwin elscreen yascroll buffer-move

    ;;;; flymake
    flycheck flymake-jslint

    ;;;; go
    go-mode

    ;;;; python
    jedi

    ;;;; helm
    helm ac-helm helm-git helm-git-files helm-descbinds

    ;;;; git
    magit git-gutter git-gutter-fringe git-blame

    ;;;; Ruby / Rails
    enh-ruby-mode rinari rspec-mode ruby-block rubocop yard-mode rhtml-mode js2-mode haml-mode
    fringe-helper ruby-electric yasnippet ruby-end scss-mode sass-mode
    yaml-mode

    ;;;; modes
    coffee-mode web-mode review-mode ghc markdown-mode scala-mode graphviz-dot-mode

    ;;;; utils
    multi-term nav auto-install
    ))

;; Install packages
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

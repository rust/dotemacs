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
    helm ac-helm helm-git helm-git-files

    ;;;; git
    magit git-gutter git-gutter-fringe

    ;;;; Ruby / Rails
    enh-ruby-mode rinari rspec-mode ruby-block rubocop yard-mode rhtml-mode js2-mode haml-mode
    fringe-helper ruby-electric yasnippet

    ;;;; modes
    coffee-mode web-mode review-mode
    ))

;; Install packages
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

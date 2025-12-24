;;; init_javascript.el --- JavaScript開発環境設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Shin-ichiro OGAWA
;; Author: Shin-ichiro OGAWA <rust@stnard.jp>

;;; Commentary:

;; JavaScriptとReactの開発環境設定。
;; RJSX-modeの設定を含む。

;;; Code:

;; for javascript

(use-package rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq js-switch-indent-offset 2)
              (setq jsx-indent-level 2)
              (setq js2-strict-missing-semi-warning nil)
              (define-key haml-mode-map "\C-m" 'newline-and-indent)))

  ;; Set 2 spaces tab
  (setq-default js2-basic-offset 2))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'"   . web-mode)
         ("\\.tsx\\'"     . web-mode)
         ("\\.jsx\\'"     . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode)
         ("\\.eex$"       . web-mode)
         ("\\.blade\\."   . web-mode))
  :config
  (setq web-mode-attr-indent-offset nil
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-sql-indent-offset 2
        indent-tabs-mode nil
        tab-width 2
        web-mode-enable-current-element-highlight t)
  (setq web-mode-engines-alist
        '(("ruby"          . "\\.erb\\'")
          ("elixir"        . "\\.eex\\'")))
  (setq web-mode-extra-keywords '(("javascript" . ("type" "interface"))))
  (add-hook 'web-mode-hook
            (lambda()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package typescript-ts-mode
  :mode (("\\\\.tsx\\\\'" . tsx-ts-mode)
         ("\\\\.ts\\\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package tide
  :ensure t
  :hook (tsx-ts-mode . setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setq company-tooltip-align-annotations t))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq vue-html-tab-width 2))
(use-package vue-html-mode
  :ensure t)

(provide 'init_javascript)
;; init_javascript.el ends here

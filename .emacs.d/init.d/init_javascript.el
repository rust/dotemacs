;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; init_javascript.el

;; Copyright (C) 2016 Shin-ichiro OGAWA
;;   Author  : Shin-ichiro OGAWA <rust@stnard.jp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for javascript

(use-package rjsx-mode
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

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package company
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip))

(use-package web-mode
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


(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide
  :init
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'init_javascript)
;; init_javascript.el ends here

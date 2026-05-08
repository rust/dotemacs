;;; init_javascript.el --- JavaScript開発環境設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; JavaScriptとReactの開発環境設定。
;; Emacs 29+ 組み込みの js-ts-mode / tsx-ts-mode を使用。

;;; Code:

;; for javascript

;; js-ts-mode（Emacs 29+ 組み込み）: .js / .jsx
(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))  ; JSX は tsx-ts-mode が担当
  :config
  (setq js-indent-level 2))

(use-package prettier-js
  :ensure t
  :defer t
  :config
  (add-hook 'js-ts-mode-hook  'prettier-js-mode)
  (add-hook 'typescript-ts-mode-hook 'prettier-js-mode)
  (add-hook 'tsx-ts-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook    'prettier-js-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
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
  (setq web-mode-extra-keywords '(("javascript" . ("type" "interface")))))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
          ("\\.ts\\'"  . typescript-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; vue-mode は放棄済み、vue-ts-mode は MELPA 未収録のため web-mode で代替
;; vue-ts-mode が MELPA に収録された時点で移行予定
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(provide 'init_javascript)
;; init_javascript.el ends here

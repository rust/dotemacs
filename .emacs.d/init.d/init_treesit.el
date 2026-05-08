;;; init_treesit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; init_treesit.el
;; Emacs 29 内蔵の treesit を使用する。
;; 外部パッケージ tree-sitter / tree-sitter-langs は削除済み（内蔵と重複するため）。

;; Copyright (C) 2025 Shin-ichiro OGAWA
;;   Author  : Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for treesit (Emacs 29+ 内蔵)

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  ;; 自動インストールは手動実行時のみ行う（起動時の自動ダウンロードを防ぐ）
  (setq treesit-auto-install 'prompt))

;; grammar のインストール：未インストールのものだけ対象とし、起動時に通信しない
(setq treesit-language-source-alist
      '((json       "https://github.com/tree-sitter/tree-sitter-json")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex       "https://github.com/phoenixframework/tree-sitter-heex")
        (kotlin     "https://github.com/nickel-lang/tree-sitter-kotlin")))

;; 未インストールの grammar を一括インストールするユーティリティ
;; 初回セットアップ時に M-x my/treesit-install-missing-grammars で実行
(defun my/treesit-install-missing-grammars ()
  "未インストールの treesit grammar をインストールする。"
  (interactive)
  (dolist (element treesit-language-source-alist)
    (let ((lang (car element)))
      (unless (treesit-language-available-p lang)
        (message "treesit: installing %s..." lang)
        (treesit-install-language-grammar lang)))))

(provide 'init_treesit)
;;; init_treesit.el ends here

;;; init_finder.el --- ファイル検索・選択の設定  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Shin-ichiro OGAWA
;; Author: Shinichiro OGAWA <rust.stnard@gmail.com>

;;; Commentary:

;; Vertico + Orderless + Marginalia + Consult によるミニバッファ補完スタック。
;; ido の代替として以下を提供する：
;;   vertico   - 縦リスト表示の補完 UI
;;   orderless - 部分一致・スペース区切りあいまい補完（ido 感覚の操作）
;;   marginalia - 補完候補にアノテーション（説明・モードなど）を付加
;;   consult   - バッファ切替・ファイル検索など強化コマンド群

;;; Code:

(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :demand t
  :bind
  (("C-x b"   . consult-buffer)        ; ido-switch-buffer の代替
   ("C-x C-f" . find-file)             ; 標準のまま（vertico が補完）
   ("M-y"     . consult-yank-pop)      ; kill-ring から貼り付け
   ("C-s"     . consult-line)          ; バッファ内インクリメンタル検索
   ("C-c g"   . consult-ripgrep)))     ; プロジェクト全体 grep

(provide 'init_finder)
;; init_finder.el ends here

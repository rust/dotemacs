;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs
;; font
;; (set-default-font "M+2VM+IPAG circle-8")
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   '("M+2VM+IPAG circle" . "unicode-bmp"))

(cond
  (window-system
    ;; デフォルトフォントの設定
    ;; フォント名-フォントサイズで指定する。
    (set-default-font "VL ゴシック-12")
    ;; 日本語(japanese-jisx0208)フォントの設定
    (set-fontset-font
      (frame-parameter nil 'font)
      'japanese-jisx0208
      '("VL ゴシック" . "unicode-bmp")
    )
  )
)

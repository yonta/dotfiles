;;; Emacs23以降のフォント設定
(cond ((>= (string-to-number emacs-version) 23)
        (cond (window-system
;               (set-frame-font "Bitstream Vera Sans Mono-11")
;               (set-frame-font "VL ゴシック-10")
               (set-frame-font "VL ゴシック-12")
;               (set-frame-font "M+2VM+IPAG circle-12")
;               (set-frame-font "Migu 1M-7")
;               (set-frame-font "MigMix 1M-7")
;               (set-frame-font "MigMix 2M-7")
               (set-fontset-font
                (frame-parameter nil 'font)
                'japanese-jisx0208
                '("VL ゴシック" . "unicode-bmp")
;                '("VL Gothic" . "unicode-bmp")
;                '("VL ゴシック:weight=bold" . "unicode-bmp")
;                '("M+2VM+IPAG circle" . "unicode-bmp")
;                '("Migu 1M" . "unicode-bmp")
;                '("MigMix 1M" . "unicode-bmp")
;                '("MigMix 2M" . "unicode-bmp")
     )))))

;;; フォントカラーの変更
(if window-system
    (progn
      (set-face-foreground 'font-lock-comment-face "Sienna") ; comment
      (set-face-foreground 'font-lock-string-face "forest green") ; string
      (set-face-foreground 'font-lock-keyword-face "purple") ; reserved word
      (set-face-foreground 'font-lock-function-name-face "blue") ; fun name
      (set-face-foreground 'font-lock-variable-name-face "dodger blue") ; val
      (set-face-foreground 'font-lock-type-face "chocolate") ; type
      (set-face-foreground 'font-lock-builtin-face "deep pink") ; ex.macro in C
      (set-face-foreground 'font-lock-constant-face "slate gray") ; ex.NULL in C
      ; (set-face-bold 'font-lock-constant-face t)
      (set-face-foreground 'font-lock-warning-face "red") ; warning
      ;(set-face-bold-p 'font-lock-warning-face nil)
      ))

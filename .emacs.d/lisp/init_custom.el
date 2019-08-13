;;; custom --- customization for each PC

;;; Commentary:
;; Customization for each PC to separate git diff.

;;; Code:

;;; ウィンドウについての設定
(setq default-frame-alist
      (append (list '(top . 0) ; 起動時の表示位置（上から）
                    '(left . 0) ; 起動時の表示位置（左から）
                    '(width . 194) ; 起動時のサイズ（幅）
                    '(height . 47) ; 起動時のサイズ（縦）
                    ;'(foreground-color . "#FFFFFF") ; 文字の色
                    ;'(background-color . "gray30") ; 背景の色
                    ;'(cursor-color . "gray") ; カーソルの色
                    )
              default-frame-alist))

;;; フォント設定
(if window-system
    (add-to-list 'default-frame-alist '(font . "VL ゴシック-12" )))

;;; init_custom.el ends here

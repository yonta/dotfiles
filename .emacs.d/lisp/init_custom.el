;;; custom --- customization for each PC

;;; Commentary:
;; Customization for each PC to separate git diff.

;;; Code:

;;; ウィンドウについての設定
(setq default-frame-alist
      (append (list
               '(top . 60) ; 起動時の表示位置（上から）
               '(left . 5) ; 起動時の表示位置（左から）
               '(width . 171) ; 起動時のサイズ（幅）
               '(height . 38) ; 起動時のサイズ（縦）
               ;;'(foreground-color . "#101010") ; 文字の色
               ;;'(background-color . "#F0F0F0") ; 背景の色
               ;;'(cursor-color . "gray") ; カーソルの色
               )
              default-frame-alist))

;;; フォント設定
(if window-system
    (add-to-list 'default-frame-alist '(font . "VL ゴシック-12" )))

;;; flycheck-smlsharp
(add-to-list 'load-path "~/git/flycheck-smlsharp/")
(require 'flycheck-smlsharp)

;;; ブラウザ設定
;; aptでubuntu-wslをいれておく
(eval-when-compile (require 'use-package))
(use-package browse-url
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "wslview"))

;;; init_custom.el ends here

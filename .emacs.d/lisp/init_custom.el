;;; custom --- customization for each PC

;;; Commentary:
;;
;; Customization for each PC to separate git diff.
;;
;; If you want that a changes of this file is not showen in `git diff',
;; use below command.
;;   git update-index --skip-worktree init_custom.el
;; And, revert by below command.
;;   git update-index --no-skip-worktree init_custom.el
;; A reference is here
;;   https://qiita.com/usamik26/items/56d0d3ba7a1300625f92

;;; Code:

;;; ウィンドウについての設定
(setq default-frame-alist
      (append (list
               '(top . 0) ; 起動時の表示位置（上から）
               '(left . 0) ; 起動時の表示位置（左から）
               '(width . 194) ; 起動時のサイズ（幅）
               '(height . 47) ; 起動時のサイズ（縦）
               ;;'(foreground-color . "#FFFFFF") ; 文字の色
               ;;'(background-color . "gray30") ; 背景の色
               ;;'(cursor-color . "gray") ; カーソルの色
               )
              default-frame-alist))

;;; フォント設定
(if window-system
    (add-to-list 'default-frame-alist '(font . "VL ゴシック-12" )))

;;; init_custom.el ends here

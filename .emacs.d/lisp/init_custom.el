;;; custom --- customization for each PC -*- lexical-binding: t; -*-

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
      ;; 起動時の表示位置とサイズ。上から、左から、幅、高さの順
      (append (list '(top . 0) '(left . 0) '(width . 194) '(height . 47))
              default-frame-alist)))

;;; フォント設定
(if window-system
    (add-to-list 'default-frame-alist '(font . "VL ゴシック-12")))

;;; カラーテーマの設定
(setq custom-theme-directory "~/.emacs.d/themes")
(if window-system (load-theme 'original t))

;;; フリンジのサイズを調整する
(fringe-mode '(15 . 10))

;;; init_custom.el ends here

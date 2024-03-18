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

;;; 行間スペース
(setq-default line-spacing 0)

;;; カラーテーマの設定
(setq custom-theme-directory "~/.emacs.d/themes")
(if window-system (load-theme 'original t))

;;; フリンジのサイズを調整する
(fringe-mode '(15 . 10))

;;; init_custom.el ends here

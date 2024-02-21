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

;;; フォント
(push '(font . "VL ゴシック-23") default-frame-alist)

;;; 行間スペース
(setq-default line-spacing 0)

;;; 起動時の表示位置とサイズ。上から、左から、幅、高さ
(push '(top . 0) default-frame-alist)
(push '(left . 0) default-frame-alist)
(push '(width . 232) default-frame-alist)
(push '(height . 63) default-frame-alist)

;;; カラーテーマの設定
(setq custom-theme-directory "~/.emacs.d/themes")
(if window-system (load-theme 'original t))

;;; フリンジのサイズを調整する
(fringe-mode '(15 . 10))

;;; カラー絵文字を表示する
;; ./configure --with-cairo でビルドし、NotoColorEmojiをいれると使える
;; https://tsuu32.hatenablog.com/entry/2019/06/20/113923
(set-fontset-font t '(#x1F000 . #x1FAFF) "Noto Color Emoji")

;;; init_custom.el ends here

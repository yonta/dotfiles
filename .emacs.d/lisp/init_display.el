;;; init_display.el --- settings for display -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about display.

;;; Code:

;;; スクロールバーを右側に配置する
(set-scroll-bar-mode 'right)

;;; 対応する括弧を光らせる
(show-paren-mode t)

;;; 同じ名前のバッファをわかりやすく名前付けする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; マウスの色を変える
(set-mouse-color "blue")

;;; init_display.el ends here

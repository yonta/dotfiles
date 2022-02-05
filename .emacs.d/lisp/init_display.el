;;; init_display.el --- settings for display -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about display.

;;; Code:

;;; 行番号と列番号を表示する
(line-number-mode t)
(column-number-mode t)

;;; Emacs27からearly-initを使う
(if (version< emacs-version "27")
    (progn
      ;; ツールバーの表示を消す
      (tool-bar-mode 0)
      ;; メニューバーの表示を消す
      (menu-bar-mode 0)))

;;; スクロールバーを右側に配置する
(set-scroll-bar-mode 'right)

;;; 対応する括弧を光らせる
(show-paren-mode t)

;;; ビープ音を消し、画面がフラッシュしないようにする
(setq ring-bell-function 'ignore)

;;; 長い行を折り返して表示する
(setq truncate-partial-width-windows nil)

;;; 同じ名前のバッファをわかりやすく名前付けする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; scratchバッファのメッセージをなくす
(setq initial-scratch-message "")

;;; マウスの色を変える
(set-mouse-color "blue")

;;; init_display.el ends here

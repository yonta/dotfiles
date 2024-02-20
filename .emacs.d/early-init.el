;;; early-init.el --- settings before init.el        -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my early init file.

;;; Code:

;;; メニューバーを表示しない
(push '(menu-bar-lines . 0) default-frame-alist)

;;; ツールバーを表示しない
(push '(tool-bar-lines . 0) default-frame-alist)

;;; 起動時に暗黙の画面サイズでチカチカさせない
(setq frame-inhibit-implied-resize t)

;;; ネイティブコンパイル時にでる大量の警告を表示しない
(custom-set-variables '(warning-suppress-types '((comp))))

;;; lsp-mode向け高速化、lsp-modeインストール時に必要
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here

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

;;; early-init.el ends here

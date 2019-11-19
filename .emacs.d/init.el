;;; init.el --- dot emacs file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my dot Emacs file.

;;; Code:

(setq gc-cons-threshold 134217728)
(require 'cl-lib)

;;; package
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

;; 初回起動時はパッケージリストがなくエラーが出るのでパッケージリストを取得
(if (not (file-exists-p "~/.emacs.d/elpa")) (package-refresh-contents))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; 自分のカスタムemacs lispのpath
;; 参考： https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; 設定ファイルの読み込み
(load "init_package")
(load "init_display")
(load "init_keybind")
(load "init_behavior")
(load "init_mode")
(load "init_custom")

;;; カスタム変数は別ファイルに保存する
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

;;; init.el ends here

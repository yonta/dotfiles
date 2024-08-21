;;; early-init.el --- settings before init.el -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; This is my early init file.

;;; Code:

;;; フレーム位置、上から・左から
(push '(top . 0) default-frame-alist)
(push '(left . 0) default-frame-alist)

;; フレームサイズ、、幅、高さ
(push '(width . 421) default-frame-alist)
(push '(height . 114) default-frame-alist)

;; フレームの最大化
;; (push '(fullscreen . maximized) default-frame-alist)
;; (push '(fullscreen . fullboth) default-frame-alist)

;;; メニューバーを表示しない
(push '(menu-bar-lines . 0) default-frame-alist)

;;; ツールバーを表示しない
(push '(tool-bar-lines . 0) default-frame-alist)

;;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;;; バックアップファイルを作らない
(setq make-backup-files nil)

;;; 自動保存リストを作らない
(setq auto-save-list-file-prefix nil)

;;; #スタートのロックファイルを作らない
(setq create-lockfiles nil)

;;; GCの閾値を上げる
(setq gc-cons-threshold (eval-when-compile (* 128 1024 1024)))

;;; ビープ音を消し、画面がフラッシュしないようにする
(setq ring-bell-function 'ignore)

;;; kill-ringを大きくする
(setq kill-ring-max 5000)

;;; kill-ringを保存
(custom-set-variables '(savehist-additional-variables '(kill-ring)))

;;; 長い行を折り返して表示する
(setq truncate-partial-width-windows nil)

;;; scratchバッファのメッセージをなくす
(setq initial-scratch-message nil)

;;; 重複履歴を削除
(setq history-delete-duplicates t)

;;; 起動時に暗黙の画面サイズでチカチカさせない
(setq frame-inhibit-implied-resize t)

;;; ネイティブコンパイル時にでる大量の警告を表示しない
(custom-set-variables '(warning-suppress-types '((comp))))

;;; lsp-mode向け高速化、lsp-modeインストール時に必要
(setenv "LSP_USE_PLISTS" "true")

;;; package-quickstartを使う
;; 何らかのエラーが起きたらpackage-quickstart-refreshを実行する必要があるかも
;; (setq package-quickstart t)

;;; auto-compile
;; .elcが古ければauto-compileする
;; 初回起動時はインストールされてないため、次回から有効になる
(setq load-prefer-newer t)
(if (package-installed-p 'auto-compile)
    (progn
      (add-to-list 'load-path "~/.config/emacs/el-get/auto-compile/")
      (require 'auto-compile)
      (auto-compile-on-load-mode)))

;;; early-init.el ends here

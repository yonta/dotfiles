;;; init_behavior.el --- settings about behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about behavior

;;; Code:

;;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;;; emacs lispの変数評価結果の表示桁数制限をなくす，デフォルトは(12,4)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; file名補完で大文字・小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; バックアップファイルを作らない
(setq make-backup-files nil)

;;; 画像ファイルを表示する
(auto-image-file-mode t)

;;; grep-modeなどdefaultをjp utf-8にする
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)

;;; dired-modeでOSによってutf-8/shift-jisを使う
(if (equal system-type 'windows-nt)
    (setq default-file-name-coding-system 'shift_jis)
  (setq default-file-name-coding-system 'utf-8))

;;; Emacsサーバーを起動する
(require 'server)
(unless (server-running-p) (server-start))

;;; Emacsのバッファ・ウィンドウ状態を保存・復元する
(desktop-save-mode 1)

;;; マーク記憶数を増やす
(setq global-mark-ring-max 512)
(setq mark-ring-max 512)

;;; pop-markを連続するときはC-u C-SPC C-SPC...にする
(setq set-mark-command-repeat-pop t)

;;; 同じ内容を重複してkill-ringにいれない
(setq kill-do-not-save-duplicates t)

;;; カーソル移動時にページ送りを少しずつ行う
(setq scroll-conservatively 1)

;;; カーソル移動のページ送りを、画面端2行前から行う
(setq scroll-margin 2)

;;; 画面スクロールでカーソル位置を変えないようにする
(setq scroll-preserve-screen-position :always)

;;; init_behavior.el ends here

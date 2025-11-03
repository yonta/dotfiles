;;; init_behavior.el --- settings about behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about behavior

;;; Code:

;;; emacs lispの変数評価結果の表示桁数制限をなくす，デフォルトは(12,4)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; file名補完で大文字・小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

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

;;; マーク記憶数を増やす
(setq global-mark-ring-max 512)
(setq mark-ring-max 512)

;;; pop-markを連続するときはC-u C-SPC C-SPC...にする
(setq set-mark-command-repeat-pop t)

;;; 同じ内容を重複してkill-ringにいれない
(setq kill-do-not-save-duplicates t)

;;; カーソルによる画面スクロールをスムーズにする
;; MEMO: デフォルトの画面スクロールは、一気にページ送りして
;;       カーソル位置をバッファ中央にするため、視線移動が大きく使いづらい。
;; カーソルを中央に戻さない
(setq scroll-conservatively 100)
;; カーソル位置が画面端より2行手前からスクロール開始
(setq scroll-margin 2)
;; 1行ずつスクロール
(setq scroll-step 1)
;; スクロール時にカーソルの位置を変えない
(setq scroll-preserve-screen-position :always)

;;; カーソルとマウスポインタが近づいたらマウスポインタを移動する
(mouse-avoidance-mode 'exile)

;;; Emacsのプロセス通信の容量を上げる
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (eval-when-compile (* 1024 1024)))

;;; init_behavior.el ends here

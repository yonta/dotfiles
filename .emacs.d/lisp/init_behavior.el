;;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;;; emacs lispの変数評価結果の表示桁数制限をなくす，デフォルトは(12,4)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; file名補完で大文字・小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 問い合わせを簡略化 yes/no を y/n
(fset 'yes-or-no-p 'y-or-n-p)

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

;;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;;; emacs lispの変数評価結果の表示桁数制限をなくす，デフォルトは(12,4)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; file名補完で大文字・小文字を区別しない
(setq completion-ignore-case t)

;;; 問い合わせを簡略化 yes/no を y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; バックアップファイルを"~/.emacs.d/backup"に集める
; backupフォルダの作成
(defvar my-backup-dir "~/.emacs.d/backup")
(unless (file-directory-p my-backup-dir) (make-directory my-backup-dir t))
; バックアップファイルを設定
(setq make-backup-file t)
(if (and (boundp 'my-backup-dir)
         (not (fboundp 'make-backup-file-name-original)))
    (progn
      (fset 'make-backup-file-name-original
            (symbol-function 'make-backup-file-name))
      (defun make-backup-file-name (filename)
        (if (and (file-exists-p (expand-file-name my-backup-dir))
                 (file-directory-p (expand-file-name my-backup-dir)))
            (concat (expand-file-name my-backup-dir)
                    "/" (file-name-nondirectory filename))
          (make-backup-file-name-original filename)))))

;;; バージョンコントロールでバックアップファイルを作らないようにする
;;; 現在はなぜか効果がでない
(setq version-control nil)

;;; タイムスタンプを自動で書き込む [Time-stamp: <##Time##>]
(if (not (memq 'time-stamp write-file-hooks))
(setq write-file-hooks
(cons 'time-stamp write-file-hooks)))

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

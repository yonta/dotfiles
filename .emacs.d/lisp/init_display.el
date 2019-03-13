;;; パスワードを隠す
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;; 行番号と列番号を表示する
(line-number-mode t)
(column-number-mode t)

;;; ツールバーの表示を消す
(tool-bar-mode 0)

;;; メニューバーの表示を消す
(menu-bar-mode 0)

;;; スクロールバーを右側に配置する
(set-scroll-bar-mode 'right)

;;; フリンジのサイズを調整する
(fringe-mode '(15 . 10))

;;; 対応する括弧を光らせる
(show-paren-mode t)

;;; マークセット部分を色付けする
(transient-mark-mode t)

;;; ビープ音を消し、画面がフラッシュしないようにする
(setq ring-bell-function 'ignore)

;;; フォントカラーの変更
(if window-system
    (progn
      (set-face-foreground 'font-lock-comment-face "Sienna") ; comment
      (set-face-foreground 'font-lock-string-face "forest green") ; string
      (set-face-foreground 'font-lock-keyword-face "purple") ; reserved word
      (set-face-foreground 'font-lock-function-name-face "blue") ; fun name
      (set-face-foreground 'font-lock-variable-name-face "dodger blue") ; val
      (set-face-foreground 'font-lock-type-face "chocolate") ; type
      (set-face-foreground 'font-lock-builtin-face "deep pink") ; ex.macro in C
      (set-face-foreground 'font-lock-constant-face "slate gray") ; ex.NULL in C
      ; (set-face-bold 'font-lock-constant-face t)
      (set-face-foreground 'font-lock-warning-face "red") ; warning
      ;(set-face-bold-p 'font-lock-warning-face nil)
      ))

;;; 長い行を折り返して表示する
(setq truncate-partial-width-windows nil)

;;; 同じ名前のバッファをわかりやすく名前付けする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 行番号を常に表示する
(global-linum-mode t)
(setq linum-format "%4d ")

;;; カーソルがいる行をハイライトする
;; '-nw'で起動時は文字が見えなくなるのでオフにする
(cond (window-system
       (require 'highlight-current-line)
       (highlight-current-line-on t)
       (set-face-background 'highlight-current-line-face "gray92")
       ))

;;; hiwin mode
;; アクティブかどうかでバッファーのモードラインの色を変える
;; (hiwin-activate)
;; (set-face-background 'hiwin-face "gray92")
(set-face-attribute 'mode-line nil :background "light sky blue")
(set-face-attribute 'mode-line-inactive nil
                    :background "light gray"
                    :foreground "dim gray")

;;; whitespace mode
;; 空白・カラムオーバーの可視化と不要な空白や改行の自動削除をする
(require 'whitespace)

;; 対象はタブ、行末スペース、カスタムスペース（全角スペース）
(setq whitespace-style '(face tabs trailing spaces empty))

;; 保存前に自動でクリーンアップ、対象はwhitespace-styleでセットしたもの
(setq whitespace-action '(auto-cleanup))

;; white spaceをオン
(global-whitespace-mode t)

;; spacesの対象は全角スペースのみ
(setq whitespace-space-regexp "\\(　+\\)")

;; 行末スペースの色
(set-face-attribute 'whitespace-trailing nil :background "Lavender")

;; 全角スペースの色
(set-face-attribute 'whitespace-space nil :background "DarkSeaGreen1")

;; タブの色
(set-face-attribute 'whitespace-tab nil :background "LightGoldenrodYellow")

;; 空行の色
(set-face-attribute 'whitespace-empty nil :background nil)

;;; fci-modeで80文字の箇所に線を引く
;; fci-modeとtruncateによる折り返しが相性悪いので対処する
;; https://www.emacswiki.org/emacs/FillColumnIndicator#toc17
;; fci-modeの不具合回避のため、幅81からfciをオンではなく、幅82からにした。
(setq fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(defun auto-fci-mode (&optional UNUSED)
  "Enable and disable fci-mode automatically by window width.
All arguments UNUSED is ignored."
  (if (> (window-width) (+ fci-rule-column 1)) (fci-mode 1) (fci-mode 0)))
(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-configuration-change-hook 'auto-fci-mode)
(add-hook 'buffer-list-update-hook 'auto-fci-mode)

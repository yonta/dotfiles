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

;;; 対応する括弧を光らせる
(show-paren-mode t)

;;; マークセット部分を色付けする
(transient-mark-mode t)

;;; ビープ音を消し、画面がフラッシュしないようにする
(setq ring-bell-function 'ignore)

;;; ウィンドウについての設定
(setq default-frame-alist
      (append (list '(top . 0) ; 起動時の表示位置（上から）
                    '(left . 0) ; 起動時の表示位置（左から）
                    '(width . 194) ; 起動時のサイズ（幅）
                    '(height . 47) ; 起動時のサイズ（縦）
                    ;'(foreground-color . "#FFFFFF") ; 文字の色
                    ;'(background-color . "gray30") ; 背景の色
                    ;'(cursor-color . "gray") ; カーソルの色
                    )
              default-frame-alist))

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

;; 対象はタブ、カラムオーバー、行末スペース、カスタムスペース（全角スペース）
;; ただし、シェルで-nw起動したときは、見づらい文字背景色の設定をオフ
(if (window-system)
    (setq whitespace-style '(face tabs lines-tail trailing spaces empty))
  (setq whitespace-style '(face tabs trailing spaces empty)))

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

;; 80文字オーバーの色
(set-face-attribute 'whitespace-line nil :foreground nil :background "Lavender")

;; java-modeではカラムオーバーの限界をデフォルトの80から100に変更する
(defun set-whitespace-line-column-80 () (setq whitespace-line-column 80))
(defun set-whitespace-line-column-100 () (setq whitespace-line-column 100))
(add-hook 'java-mode-hook 'set-whitespace-line-column-100)
(add-hook 'change-major-mode-hook 'set-whitespace-line-column-80)

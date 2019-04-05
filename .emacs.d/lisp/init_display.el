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
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 行番号を常に表示する
(if (version<= "26" emacs-version)
    (progn (global-display-line-numbers-mode 1))
  (progn (global-linum-mode t)
         (setq linum-format "%4d ")))

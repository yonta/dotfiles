;;; init_mode --- settings about mode

;;; Commentary:
;; This is settings about mode.
;; It will be migrated to init_package.

;;; Code:

;;; タブ文字を禁止してスペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;;; Sticky Buffer Mode (minor mode)、バッファを固定する
;; 参考： https://www.emacswiki.org/emacs/StickyBufferMode
;; sticky-windows.elがあるが、メンテナがいない状態らしい
;; 参考： https://www.emacswiki.org/emacs/StickyWindows
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;; init_mode.el ends here

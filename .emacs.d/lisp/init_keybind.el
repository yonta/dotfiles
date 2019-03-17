;;; init-keybinding.el --- settings about keybindigns

;;; Commentary:
;; This file contains settings set of keybindings for Emacs.

;;; C-hをバックスペースにする
(global-set-key (kbd "C-h") 'backward-delete-char)
;;; M-&を正規表現置換にする
(global-set-key (kbd "M-&") 'replace-regexp)
;;; M-?をヘルプに
(global-set-key (kbd "M-?") 'help-for-help)
;;; C-c dでカーソル位置のシンボルのヘルプを表示
(global-set-key (kbd "C-c d") 'describe-symbol)
;;; C-cgをgrep-findに
(global-set-key (kbd "C-c g") 'grep-find)
(setq grep-find-command
      `("find . -type f -print0 | xargs -0 -e grep -nHE " . 48))
;;; terminal(-nw)で起動した場合は、C-SPCが使えないので、C-]にする
(if (not window-system) (global-set-key (kbd "C-]") 'set-mark-command))
;;; C-x oの代わりのバッファ移動
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

;;; Shift + カーソル で分割ウィンドウ間を移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;;; カーソルのみで分割ウィンドウ間を移動
;; (global-set-key (kbd "<left>")  'windmove-left)
;; (global-set-key (kbd "<right>") 'windmove-right)
;; (global-set-key (kbd "<up>")    'windmove-up)
;; (global-set-key (kbd "<down>")  'windmove-down)

;;; swap-screenで上下や左右のバッファを入れ替え，これをC-Oにする
(defun swap-screen()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(global-set-key (kbd "C-x O") 'swap-screen)

;;; 分割ウィンドウのサイズを変更するwindow-resizer
; window-resizer関数
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]" (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l) (enlarge-window-horizontally dx))
              ((= c ?h) (shrink-window-horizontally dx))
              ((= c ?j) (enlarge-window dy))
              ((= c ?k) (shrink-window dy))
              ;; otherwise
              (t (message "Quit") (throw 'end-flag t)))))))
; window-resizerをC-crにセット
(global-set-key (kbd "C-c r") 'window-resizer)

;;; C-M-:に連番Evalを割り当て、ただしterminalならM-*
(defun my-insert-repeat-numbers ()
  "Insert ordered number with formatt.
A default format is start with 1, end with 10, and only number string."
  (interactive)
  (let* ((my-repeat-num-command
          "(loop for i from 1 to 10 do (insert (format \"%d\\n\" i)))"))
    (eval (read-from-minibuffer "Enter: " my-repeat-num-command nil t))))
(if (window-system)
    (global-set-key (kbd "C-M-:") 'my-insert-repeat-numbers)
  (global-set-key (kbd "C-c M-:") 'my-insert-repeat-numbers))

;;; init_keybind.el ends here

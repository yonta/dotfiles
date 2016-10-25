;;; C-hをバックスペースにする
(global-set-key "\C-h" 'backward-delete-char)
;;; M-&を正規表現置換にする
(global-set-key "\M-&" 'replace-regexp)
;;; M-?をヘルプに
(global-set-key "\M-?" 'help-for-help)
;;; C-cgをgrep-findに
(global-set-key "\C-cg" 'grep-find)
(setq grep-find-command
      `("find . -type f -print0 | xargs -0 -e grep -nHE " . 48))
;;; C-x oの代わりのバッファ移動
(global-set-key "\C-cl" 'windmove-right)
(global-set-key "\C-ch" 'windmove-left)
(global-set-key "\C-cj" 'windmove-down)
(global-set-key "\C-ck" 'windmove-up)

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
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(global-set-key "\C-xO" 'swap-screen)

;;; C-xC-bでbufferじゃなくibufferを使用する
(global-set-key "\C-x\C-b" 'ibuffer)

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
(global-set-key "\C-cr" 'window-resizer)

;;; C-M-:に連番Evalを割り当て
(setq my-repeat-num-command
      "(loop for i from 1 to 10 do (insert (format \"%d\\n\" i)))")
(defun my-insert-repeat-numbers ()
  (interactive)
  (eval (read-from-minibuffer "Enter: " my-repeat-num-command nil t)))
(global-set-key (kbd "C-M-:") 'my-insert-repeat-numbers)

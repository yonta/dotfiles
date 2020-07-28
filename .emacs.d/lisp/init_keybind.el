;;; init-keybinding.el --- settings about keybindigns -*- lexical-binding: t; -*-

;;; Commentary:
;; This is settings about keybindings.

;;; Code:
(eval-when-compile (require 'bind-key))

;;; C-hをバックスペースにする
;; global-set-keyやbind-keyより、key自体を交換するのがよい
;; https://qiita.com/takc923/items/e279f223dbb4040b1157
;; http://www.geocities.co.jp/SiliconValley-Bay/9285/EMACS-JA/emacs_461.html
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;;; M-hをバックスペース＆インデント削除にする
(bind-key "M-h" #'delete-indentation)
;;; C-c dでカーソル位置のシンボルのヘルプを表示
(bind-key "C-c d" #'describe-symbol)

;;; C-cgをgrep-findに
;; cargoでripgrepを入れておく
;; cargo install ripgrep
(bind-key "C-c f" #'grep-find)
(setq grep-find-command
      (let* ((format "rg --no-heading ")
             (point (+ (length format) 1)))
        `(,format . ,point)))

;;; terminal(-nw)で起動した場合は、C-SPCが使えないので、C-]にする
(if (not window-system) (bind-key "C-]" #'set-mark-command))

;;; swap-screenで上下や左右のバッファを入れ替え，これをC-Oにする
(defun swap-screen()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(bind-key "C-x O" #'swap-screen)

;;; 分割ウィンドウのサイズを変更するmy-window-resizer
;; my-window-resizer関数
;; https://khiker.hatenablog.jp/entry/20100119/window_resize
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action
        c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector
               (format "size[%dx%d]" (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l) (enlarge-window-horizontally dx))
              ((= c ?h) (shrink-window-horizontally dx))
              ((= c ?j) (enlarge-window dy))
              ((= c ?k) (shrink-window dy))
              ;; otherwise
              (t
               (let ((command (key-binding action)))
                 (when command (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
;; my-window-resizerをC-crにセット
(bind-key "C-c r" #'my-window-resizer)

;;; C-M-:に連番Evalを割り当て、ただしterminalならM-*
(defun my-insert-repeat-numbers ()
  "Insert ordered number with formatt.
A default format is start with 1, end with 10, and only number string."
  (interactive)
  (let* ((my-repeat-num-command
          "(let (r) (dotimes (i 10 r) (insert (format \"%d\\n\" i))))"))
    (eval (read-from-minibuffer "Enter: " my-repeat-num-command nil t))))
(if (window-system)
    (bind-key "C-M-:" #'my-insert-repeat-numbers)
  (bind-key "C-c M-:" #'my-insert-repeat-numbers))

;;; init_keybind.el ends here

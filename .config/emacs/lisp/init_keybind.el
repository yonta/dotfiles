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
(bind-key "C-c d" #'describe-symbol 'emacs-lisp-mode-map)
(bind-key "C-c d" #'describe-symbol 'lisp-interaction-mode-map)

;;; terminal(-nw)で起動した場合は、C-SPCが使えないので、C-]にする
(if (not window-system) (bind-key "C-]" #'set-mark-command))

;;; C-M-:に連番Evalを割り当て、ただしterminalならC-c M-:
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

(defun my/next-lines ()
  "Move cursor vertically down 5 lines."
  (interactive)
  (forward-line 5))
(bind-key "M-<down>" #'my/next-lines)

(defun my/previous-lines ()
  "Move cursor vertically up 5 lines."
  (interactive)
  (forward-line -5))
(bind-key "M-<up>" #'my/previous-lines)

;;; suspend-frameを無効化
;; WSLやemコマンドでは復帰できなくなる
(bind-key "C-z" nil)
(bind-key "C-x z" nil)

;;; init_keybind.el ends here

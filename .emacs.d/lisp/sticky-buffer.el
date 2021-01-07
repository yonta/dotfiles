;;; sticky-buffer.el --- a minor mode to fix buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 SAITOU Keita
;; Author: SAITOU Keita <keita44.f4@gmail.com>
;; Keywords: tools

;; This file is licensed under the GPLv2
;; The original source is here
;; https://www.emacswiki.org/emacs/StickyBufferMode
;; It is distributed under the GPLv2.

;;; Commentary:

;; This forces a window to always display the same buffer, protecting it
;; from being used by `display-buffer'.

;;; Code:

;; Sticky Buffer Mode (minor mode)、バッファを固定する
;; 参考： https://www.emacswiki.org/emacs/StickyBufferMode
;; sticky-windows.elがあるが、メンテナがいない状態らしい
;; 参考： https://www.emacswiki.org/emacs/StickyWindows
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " [S]" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

(provide 'sticky-buffer)
;;; sticky-buffer.el ends here

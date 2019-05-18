;;; init.el --- dot emacs file

;;; Commentary:
;; This is my dot Emacs file.

;;; Code:

(setq gc-cons-threshold 134217728)
(require 'cl-lib)

;;; package
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; 自分のカスタムemacs lispのpath
;; 参考： https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; 設定ファイルの読み込み
(load "init_package")
(load "init_display")
(load "init_keybind")
(load "init_behavior")
(load "init_mode")
(load "init_custom")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-function-names
   (quote
    ((counsel-clj :around avy-migemo-disable-around)
     (counsel-grep :around counsel-grep-migemo-around)
     counsel-grep-function-migemo counsel-grep-occur-migemo
     (counsel-git-occur :around counsel-git-occur-migemo-around)
     (counsel-find-file-occur :around counsel-find-file-occur-migemo-around)
     swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(package-selected-packages
   (quote
    (arduino-mode company flycheck ivy migemo yasnippet counsel swiper bind-key flycheck-popup-tip which-key recentf-ext async expand-region company-c-headers volatile-highlights dumb-jump dump-jump git-gutter-fringe+ yasnippet-snippets use-package twittering-mode tuareg sml-mode smartparens rainbow-mode quickrun python-mode py-autopep8 proof-general popwin markdown-mode ivy-rich image-dired+ hiwin haxe-mode graphviz-dot-mode google-translate gnuplot-mode flycheck-ocaml edit-indirect diminish csv-mode company-quickhelp company-jedi company-arduino bash-completion avy-migemo auto-package-update auto-async-byte-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

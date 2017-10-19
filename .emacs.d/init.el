(setq gc-cons-threshold 134217728)
(require 'cl)

;;; package
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
;; elpyを追加
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; Marmaladeを追加
;; よく鯖落ちするのでオフ
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; path
;; load-pathを再帰的に追加する関数
(defun add-to-load-path (&rest paths)
(let (path)
    (dolist (path paths paths)
    (let ((default-directory (expand-file-name
                              (concat user-emacs-directory path))))
    (add-to-list 'load-path default-directory)
     (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
         (normal-top-level-add-subdirs-to-load-path))))))

;; 自分のカスタムemacs-lisp path
(add-to-load-path "lisp")
;; 自分のカスタムemacs-lispを自動でバイトコンパイルする
;(byte-recompile-directory "~/.emacs.d/lisp" 0)

;; elpa以下
(defvar my-elpa-dir "elpa")
(defvar my-elpa-path (concat "~/.emacs.d/" my-elpa-dir))
(unless (file-directory-p my-elpa-path) (make-directory my-elpa-path t))
(add-to-load-path my-elpa-dir)

;;; 設定ファイルの読み込み
(load "init_package")
(load "init_font")
(load "init_display")
(load "init_keybind")
(load "init_behavior")
(load "init_mode")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-google-cpplint google-c-style image-dired+ yasnippet company-arduino flycheck-irony irony rtags arduino-mode w3m twittering-mode swiper sml-mode smartparens rainbow-mode quickrun python-mode py-autopep8 popwin markdown-mode image+ hiwin highlight-current-line google-translate flymake-python-pyflakes flymake-cursor flycheck esup elpy dos doctags csv-nav csv-mode browse-kill-ring bash-completion))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

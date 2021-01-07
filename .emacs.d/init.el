;;; init.el --- dot emacs file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my dot Emacs file.

;;; Code:

;;; Emacs起動時の機能読み込みをログしてsvgに吐く
;; コメントアウトを外し、Emacsを起動して、
;; `initchart-visualize-init-sequence'を実行して出力先を指定する。
;; (let ((default-directory  "~/.emacs.d/el-get/initchart"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

(require 'cl-lib)

;;; GCのしきい値を上げ、アイドル時にGCしておく
(setq gc-cons-threshold (eval-when-compile (* 128 1024 1024)))
(run-with-idle-timer 300 t #'garbage-collect)

;;; .el/.elcで新しい方を読み込む
(setq load-prefer-newer t)

;;; .elcが古ければauto-compileする
;; 初回起動時はインストールされてないため、次回から有効になる
(require 'package)
(if (package-installed-p 'auto-compile)
    (progn
      (require 'auto-compile)
      (declare-function auto-compile-on-load-mode "auto-compile")
      (auto-compile-on-load-mode)))

;;; 自分のカスタムemacs lispのpath
;; 参考： https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; 設定ファイルの読み込み
(mapc #'load
      '("init_util" "init_package" "init_display" "init_keybind"
        "init_behavior" "init_custom"))

;;; カスタム変数は別ファイルに保存する
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

;;; init.el ends here

;;; init.el --- dot emacs file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my dot Emacs file.

;;; Code:

;;; 起動処理中はMagic File Name機能をオフ
;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#magic-file-name-%E3%82%92%E4%B8%80%E6%99%82%E7%9A%84%E3%81%AB%E7%84%A1%E5%8A%B9%E3%81%AB%E3%81%99%E3%82%8B
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;; Emacs起動時の機能読み込みをログしてsvgに吐く
;; コメントアウトを外し、Emacsを起動して、
;; `initchart-visualize-init-sequence'を実行して出力先を指定する。
;; (let ((default-directory  "~/.emacs.d/el-get/initchart"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

;; 起動時にのdeprecatedメッセージを隠す
;; ときどきオフにして確認したほうがよさそう
(customize-set-variable 'byte-compile-warnings '(not obsolete))

;;; アイドル時にGCを走らせる
(run-with-idle-timer (eval-when-compile (* 5 60)) t #'garbage-collect)

;;; .el/.elcで新しい方を読み込む
(setq load-prefer-newer t)

;;; .elcが古ければauto-compileする
;; 初回起動時はインストールされてないため、次回から有効になる
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

;;; 起動処理終了でMagic File Name機能を復元
(setq file-name-handler-alist my/saved-file-name-handler-alist)

;;; init.el ends here

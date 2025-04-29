;;; init.el --- dot emacs file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my dot Emacs file.

;;; Code:

;;; 起動処理中はMagic File Name機能をオフ
;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#magic-file-name-%E3%82%92%E4%B8%80%E6%99%82%E7%9A%84%E3%81%AB%E7%84%A1%E5%8A%B9%E3%81%AB%E3%81%99%E3%82%8B
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 起動処理中における各パッケージロードのタイミング・依存関係を解析する
;; コメントアウトを外してEmacsを起動するとsetup-trackerバッファに結果がでる
;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/4d0a9dde1043c6eaffad
;;
;; (defvar setup-tracker--level 0)
;; (defvar setup-tracker--parents nil)
;; (defvar setup-tracker--times nil)
;; (when load-file-name
;;   (push load-file-name setup-tracker--parents)
;;   (push (current-time) setup-tracker--times)
;;   (setq setup-tracker--level (1+ setup-tracker--level)))
;; (add-variable-watcher
;;  'load-file-name
;;  (lambda (_ v &rest __)
;;    (cond ((equal v (car setup-tracker--parents))
;;           nil)
;;          ((equal v (cadr setup-tracker--parents))
;;           (setq setup-tracker--level (1- setup-tracker--level))
;;           (let* ((now (current-time))
;;                  (start (pop setup-tracker--times))
;;                  (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
;;                              (/ (- (nth 2 now) (nth 2 start)) 1000))))
;;             (with-current-buffer (get-buffer-create "*setup-tracker*")
;;               (save-excursion
;;                 (goto-char (point-min))
;;                 (dotimes (_ setup-tracker--level) (insert "> "))
;;                 (insert
;;                  (file-name-nondirectory (pop setup-tracker--parents))
;;                  " (" (number-to-string elapsed) " msec)\n")))))
;;          (t
;;           (push v setup-tracker--parents)
;;           (push (current-time) setup-tracker--times)
;;           (setq setup-tracker--level (1+ setup-tracker--level))))))

;;; Emacs起動時の機能読み込みをログしてsvgに吐く
;; コメントアウトを外し、Emacsを起動して、
;; `initchart-visualize-init-sequence'を実行して出力先を指定する。
;;
;; (let ((default-directory  "~/.config/emacs/elpa/initchart"))
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

;;; 自分のカスタムemacs lispのpath
;; 参考： https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.config/emacs/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; 設定ファイルの読み込み
;; packageで使うsmartrepが先にテーマ色を設定している前提になっている
;; そのため、custom -> packageの順で読み込む
(mapc #'load
      '("init_util" "init_custom" "init_package" "init_display" "init_keybind"
        "init_behavior"))

;;; カスタム変数は別ファイルに保存する
(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file) (load custom-file))

;;; 起動処理終了でMagic File Name機能を復元
(setq file-name-handler-alist my/saved-file-name-handler-alist)

;;; init.el ends here

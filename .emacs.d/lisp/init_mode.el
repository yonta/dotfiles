;;; ansi-term
;; コマンドラインと同じ色付けを使う
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; タブ文字を禁止してスペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;;; Sticky Buffer Mode (minor mode)、バッファを固定する
(defvar sticky-buffer-previous-header-line-format)
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (if sticky-buffer-mode
      (progn
        (set (make-local-variable 'sticky-buffer-previous-header-line-format)
             header-line-format)
        (set-window-dedicated-p (selected-window) sticky-buffer-mode))
    (set-window-dedicated-p (selected-window) sticky-buffer-mode)
    (setq header-line-format sticky-buffer-previous-header-line-format)))

;;; autoinsertを使ってファイル作成時にテンプレートを使う
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
(setq auto-insert-alist
      (nconc '(
               ("Test\\.cpp$" . ["templateTest.cpp" my-template])
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.c$"   . ["template.c" my-template])
               ("\\.h$"   . ["template.h" my-template])
               ("\\.ino$" . ["template.ino" my-template])
               ("\\.py$" . ["template.py" my-template])
               ) auto-insert-alist))
(defvar template-replacements-alists
  '(
    ("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%without-test%" .
     (lambda ()
       ((lambda (arg)(replace-regexp-in-string "Test$" "" arg))
        (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
    ("%file-without-ext%" .
     (lambda ()
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%" .
     (lambda ()
       (format "%s_H"
               (upcase (file-name-sans-extension
                        (file-name-nondirectory buffer-file-name))))))
    ))
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;; Google Translate mode
(autoload 'google-translate "google-translate" nil t)
(autoload 'google-translate-smooth-ui "google-translate-smooth-ui" nil t)
(global-set-key (kbd "C-c C-t") 'google-translate-smooth-translate)
(global-set-key (kbd "C-c t") 'google-translate-query-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "ja") ("ja" . "en")))

;;; help-modeの設定
;; Alt+左右でヘルプの進む・戻るを行う、デフォルトはl/r
(define-key help-mode-map (kbd "M-<left>") 'help-go-back)
(define-key help-mode-map (kbd "M-<right>") 'help-go-forward)

;;; haxe-modeの設定
(require 'haxe-mode)
(add-hook 'haxe-mode-hook
          (function (lambda () (c-add-style "haxe" my-haxe-style t))))
(add-hook 'haxe-mode-hook
          (function
           (lambda ()
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (setq fill-column 80)
             )))
